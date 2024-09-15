const std = @import("std");
const os = @import("std.os");
const lsp = @import("lsp");
const c_header = @cImport({
    @cInclude("stdlib.h");
    @cInclude("termios.h");
    @cInclude("unistd.h");
    @cInclude("sys/ioctl.h");
    @cInclude("ctype.h");
    @cInclude("time.h");
    @cInclude("string.h");
});
const mem = std.mem;
const ArrayList = std.ArrayList;
const STD_FILENO = 0;
const STDOUT_FILENO = 1;
var search_query: ArrayList(u8) = undefined;
var search_index: usize = 0;
var search_direction: i8 = 1; // 1 for forward, -1 for backward
var is_searching: bool = false;
const erow = struct {
    size: u16,
    chars: ?[]u8,
};

const StatusMessage = struct {
    message: [256]u8,
    time: i64,
};
const editorconfig = struct {
    cx: u16,
    cy: u16,
    dirty: u8,
    rows: u16,
    cols: u16,
    row: []erow,
    rowoffset: u16,
    coloffset: u16,
    numrows: u16,
    filename: []const u8,
    statusmessage: []const u8,
    statusmessage_time: c_header.time_t,
    original_term: c_header.termios,
};
const abuf = struct {
    b: ?[]u8,
    len: usize,
    pub fn init() !abuf {
        return .{
            .b = null,
            .len = 0,
        };
    }
    pub fn deinit(self: *abuf, allocator: ?std.mem.Allocator) void {
        if (self.b) |buf| {
            if (allocator) |a| {
                a.free(buf);
            }
        }
    }
};

pub const keys = enum(u8) {
    PAGE_UP = 1,
    PAGE_DOWN = 2,
    HOME_KEY = 3,
    END_KEY = 4,
    DEL_KEY = 5,
    SAVE_KEY = 6,
    FIND_KEY = 7,
    ARROW_UP = 253,
    ARROW_DOWN = 254,
    ARROW_LEFT = 255,
    ARROW_RIGHT = 252,
    BACKSPACE = 127,
};

var quit_times: u16 = 1;
var editorvar = editorconfig{
    .cy = 0,
    .cx = 0,
    .rows = 0,
    .dirty = 0,
    .rowoffset = 0,
    .coloffset = 0,
    .cols = 0,
    .row = &[_]erow{},
    .numrows = 0,
    .filename = undefined,
    .original_term = c_header.termios{},
    .statusmessage_time = 0,
    .statusmessage = undefined,
};
var statusmsg = StatusMessage{ .message = undefined, .time = 0 };
var last_update_time: i64 = 0;
// Terminal functions
pub fn rawmode() !void {
    const VMIN = 5;
    const VTIME = 6;
    _ = c_header.tcgetattr(STD_FILENO, &editorvar.original_term);
    var term = editorvar.original_term;
    _ = c_header.atexit(disablerawmode);
    term.c_lflag &= ~@as(c_uint, c_header.ECHO | c_header.ICANON | c_header.IEXTEN | c_header.ISIG);
    term.c_iflag &= ~@as(c_uint, c_header.IXON | c_header.ICRNL);
    term.c_oflag &= ~@as(c_uint, c_header.OPOST);
    term.c_cc[VMIN] = 0;
    term.c_cc[VTIME] = 1;
    _ = c_header.tcsetattr(STDOUT_FILENO, c_header.TCSAFLUSH, &term);
}

pub fn disablerawmode() callconv(.C) void {
    _ = c_header.tcsetattr(STD_FILENO, c_header.TCSAFLUSH, &editorvar.original_term);
}

pub fn windowsize(rows: *u16, cols: *u16) !i32 {
    var ws: c_header.winsize = undefined;
    _ = c_header.ioctl(STDOUT_FILENO, c_header.TIOCGWINSZ, &ws);
    cols.* = ws.ws_col;
    rows.* = ws.ws_row;
    return 0;
}
pub fn writefile(allocator: std.mem.Allocator) !void {
    try removeBlankLines(allocator);
    const file = try std.fs.openFileAbsolute(editorvar.filename, .{ .mode = .write_only });
    defer file.close();
    var i: u16 = 0;
    while (i < editorvar.numrows) {
        if (editorvar.row[i].size == 0) {
            try deleteRow(i, allocator);
            continue;
        }
        if (editorvar.row[i].chars) |chars| {
            try file.writer().writeAll(chars[0..editorvar.row[i].size]);
            try file.writer().writeByte('\n');
        }
        i += 1;
    }
    editorvar.dirty = 0;

    var args = [3][]const u8{
        "./zig-out/bin/Zig",
        "/home/covix/Zig/./zls",
        "",
    };
    const argv = try std.process.argsAlloc(allocator);
    args[2] = argv[1];
    defer std.process.argsFree(allocator, argv);

    var child = std.process.Child.init(&args, allocator);
    child.stdout_behavior = .Pipe;
    try child.spawn();
    const result = try child.wait();

    const msgfile = try std.fs.openFileAbsolute("/home/covix/Zig/logoutput.txt", .{ .mode = .read_only });
    defer msgfile.close();
    const contents = try msgfile.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(contents);

    if (std.mem.indexOf(u8, contents, "[")) |first_bracket| {
        if (std.mem.lastIndexOf(u8, contents, "]")) |last_bracket| {
            const diagnostics = contents[first_bracket .. last_bracket + 1];

            const parsed = try std.json.parseFromSlice(std.json.Value, allocator, diagnostics, .{});
            defer parsed.deinit();
            if (parsed.value.array.items.len > 0) {
                var error_messages = std.ArrayList([]const u8).init(allocator);
                defer error_messages.deinit();

                for (parsed.value.array.items) |item| {
                    if (item == .object) {
                        const obj = item.object;
                        if (obj.get("range")) |range| {
                            if (range.object.get("start")) |start| {
                                const line = start.object.get("line").?.integer;
                                const message = obj.get("message").?.string;
                                const error_msg = try std.fmt.allocPrint(allocator, "line {d}: {s}", .{ line + 1, message });
                                try error_messages.append(error_msg);
                            }
                        }
                    }
                }
                var error_buffer = std.ArrayList(u8).init(allocator);
                defer error_buffer.deinit();
                for (error_messages.items) |errors| {
                    const numspaces = editorvar.cols - errors.len + 7;
                    const spaces = try allocator.alloc(u8, numspaces);
                    defer allocator.free(spaces);
                    @memset(spaces, ' ');
                    const formaterror = try std.fmt.allocPrint(allocator, "{s}", .{errors});
                    try error_buffer.appendSlice(formaterror);
                    try error_buffer.appendSlice(spaces);
                }
                const error_buffer_const: []const u8 = error_buffer.items;
                try errorbuffer(allocator, error_buffer_const);
            } else {
                try setStatusMessage("No errors in the file", 2);
            }
        }
    } else {
        try setStatusMessage("File saved successfully", 2);
    }

    switch (result) {
        .Exited => |code| {
            if (code != 0) {
                try setStatusMessage(try std.fmt.allocPrint(allocator, "Command exited with non-zero status: {d}", .{code}), 2);
            }
        },
        else => {
            try setStatusMessage("Command didn't exit normally", 2);
        },
    }
}
pub fn editoropen(allocator: std.mem.Allocator, filename: []const u8) !void {
    if (std.mem.eql(u8, filename, "Untitled") == true) {
        editorvar.filename = try allocator.dupe(u8, filename);
    } else {
        editorvar.filename = try allocator.dupe(u8, filename);
        const file = try std.fs.openFileAbsolute(filename, .{ .mode = .read_write });
        // Output functions
        defer file.close();
        const n: u16 = 28;
        var i: u16 = 0;
        var readline: ?[1024]u8 = undefined;
        while (i < n) {
            const line = try file.reader().readUntilDelimiterOrEof(&readline.?, '\n');
            if (line) |str| {
                const temp: u16 = editorvar.numrows;
                editorvar.row = try allocator.realloc(editorvar.row, editorvar.numrows + 1);
                editorvar.row[temp].size = @intCast(str.len);
                editorvar.row[temp].chars = try allocator.alloc(u8, str.len);
                @memcpy(editorvar.row[temp].chars.?, str);
                i += 1;
                editorvar.numrows += 1;
            }
        }
    }
}

pub fn initeditor(allocator: std.mem.Allocator) !void {
    editorvar.cx = 0;
    editorvar.cy = 0;
    editorvar.numrows = 0;
    editorvar.coloffset = 0;
    editorvar.dirty = 0;
    editorvar.rowoffset = 0;
    editorvar.row = try allocator.alloc(erow, 0);
    _ = try windowsize(&editorvar.rows, &editorvar.cols);
    editorvar.rows -= 1; // Make room for status bar
    editorvar.cols -= 7; // Make room for line numbers (4 digits + space + bar + space)
    editorvar.filename = undefined;
    editorvar.statusmessage = undefined;
    editorvar.statusmessage_time = 0;
}

pub fn abappend(self: *abuf, allocator: ?std.mem.Allocator, string: []const u8) !void {
    if (self.b) |buf| {
        if (allocator) |a| {
            self.b = try a.realloc(buf, self.len + string.len);
        }
        std.mem.copyForwards(u8, self.b.?[self.len..], string);
    } else {
        if (allocator) |a| {
            self.b = try a.alloc(u8, string.len);
        }
        std.mem.copyForwards(u8, self.b.?, string);
    }
    self.len += string.len;
}

pub fn drawLineNumbers(buf: *abuf, allocator: std.mem.Allocator) !void {
    var y: usize = 0;
    while (y < editorvar.rows) : (y += 1) {
        const filerow: usize = y + editorvar.rowoffset;
        try abappend(buf, allocator, "\x1b[K"); // Clear the line
        if (filerow < editorvar.numrows) {
            var line_num_buf: [5]u8 = undefined; // 4 digits + null terminator
            const line_num = try std.fmt.bufPrint(&line_num_buf, "{d:4}", .{filerow + 1});
            try abappend(buf, allocator, "\x1b[7m"); // Invert colors
            try abappend(buf, allocator, line_num);
            try abappend(buf, allocator, "\x1b[m"); // Reset colors
        } else {
            try abappend(buf, allocator, "    ");
        }
        try abappend(buf, allocator, " │ ");
    }
}
pub fn enter(allocator: std.mem.Allocator) !void {
    if (editorvar.cx == 0) {
        try insertRow(editorvar.cy, "", allocator);
    } else {
        var row = &editorvar.row[editorvar.cy];
        try insertRow(editorvar.cy + 1, row.chars.?[editorvar.cx..row.size], allocator);
        row = &editorvar.row[editorvar.cy];
        row.size = editorvar.cx;
        row.chars = try allocator.realloc(row.chars.?, row.size);
    }
    editorvar.cy += 1;
    editorvar.cx = 0;
}
pub fn insertRow(at: u16, s: []const u8, allocator: std.mem.Allocator) !void {
    if (at > editorvar.numrows) return;

    editorvar.row = try allocator.realloc(editorvar.row, editorvar.numrows + 1);
    std.mem.copyBackwards(erow, editorvar.row[at + 1 .. editorvar.numrows + 1], editorvar.row[at..editorvar.numrows]);

    editorvar.row[at] = erow{
        .size = @intCast(s.len),
        .chars = try allocator.dupe(u8, s),
    };

    editorvar.numrows += 1;
}
pub fn appendRow(at: u16, s: []const u8, allocator: std.mem.Allocator) !void {
    var row = &editorvar.row[at];
    const new_size = row.size + s.len;
    row.chars = try allocator.realloc(row.chars.?, new_size);
    @memcpy(row.chars.?[row.size..new_size], s);
    row.size = new_size;
}

pub fn deleteRow(at: u16, allocator: std.mem.Allocator) !void {
    if (at >= editorvar.numrows) return;

    allocator.free(editorvar.row[at].chars.?);
    @memcpy(editorvar.row[at .. editorvar.numrows - 1], editorvar.row[at + 1 .. editorvar.numrows]);
    editorvar.numrows -= 1;
}
// Input functions
pub fn ctrl_key(k: u8) u8 {
    const bitwise: u8 = (k & 0x1f);
    return bitwise;
}
const HighlightingRules = struct {
    keywords: []const u8, // List of keywords as a single string with space-separated words
    keyword_color: []const u8, // ANSI code for keyword color
    comment: []const u8, // ANSI code for comment color
    string: []const u8, // ANSI code for string color
};

const languages = [_]HighlightingRules{
    HighlightingRules{
        .keywords = "if else for while fn pub const var while return defer",
        .keyword_color = "\x1b[38;5;161m", // blue
        .comment = "\x1b[90m", // gray
        .string = "\x1b[32m", // green
    },
};

pub fn highlightText(text: []const u8, language: u8, allocator: std.mem.Allocator) anyerror![]const u8 {
    const rules = languages[language];
    var highlighted_text = try allocator.alloc(u8, text.len * 8); // Increased buffer size to accommodate ANSI escape codes

    var i: usize = 0;
    var highlighted_text_index: usize = 0;

    while (i < text.len) {
        if (text[i] == '/' and i + 1 < text.len and text[i + 1] == '/') {
            // Comment
            for (rules.comment) |char| {
                highlighted_text[highlighted_text_index] = char;
                highlighted_text_index += 1;
            }
            i += 2;
            while (i < text.len and text[i] != '\n') {
                highlighted_text[highlighted_text_index] = text[i];
                highlighted_text_index += 1;
                i += 1;
            }
            // Reset the color
            highlighted_text[highlighted_text_index] = '\x1b';
            highlighted_text_index += 1;
            highlighted_text[highlighted_text_index] = '[';
            highlighted_text_index += 1;
            highlighted_text[highlighted_text_index] = '0';
            highlighted_text_index += 1;
            highlighted_text[highlighted_text_index] = 'm';
            highlighted_text_index += 1;
        } else if (text[i] == '"' or text[i] == '\'') {
            // String
            for (rules.string) |char| {
                highlighted_text[highlighted_text_index] = char;
                highlighted_text_index += 1;
            }
            const quote = text[i];
            highlighted_text[highlighted_text_index] = quote; // Add the quote character
            highlighted_text_index += 1;
            i += 1;
            while (i < text.len and text[i] != quote) {
                highlighted_text[highlighted_text_index] = text[i];
                highlighted_text_index += 1;
                i += 1;
            }
            highlighted_text[highlighted_text_index] = quote; // Add the quote character
            highlighted_text_index += 1;
            i += 1;
            // Reset the color
            highlighted_text[highlighted_text_index] = '\x1b';
            highlighted_text_index += 1;
            highlighted_text[highlighted_text_index] = '[';
            highlighted_text_index += 1;
            highlighted_text[highlighted_text_index] = '0';
            highlighted_text_index += 1;
            highlighted_text[highlighted_text_index] = 'm';
            highlighted_text_index += 1;
        } else {
            // Check for keywords
            var keyword_found = false;
            var j: usize = i;
            while (j < text.len and std.ascii.isAlphanumeric(text[j])) : (j += 1) {}
            const word = text[i..j];
            var iterator = std.mem.split(u8, rules.keywords, " ");
            while (iterator.next()) |keyword| {
                if (std.mem.eql(u8, word, keyword)) {
                    // Check if the word is surrounded by non-alphanumeric characters
                    var prev_char: u8 = ' ';
                    if (i > 0) {
                        prev_char = text[i - 1];
                    }
                    var next_char: u8 = ' ';
                    if (j < text.len) {
                        next_char = text[j];
                    }
                    if (!std.ascii.isAlphanumeric(prev_char) and !std.ascii.isAlphanumeric(next_char)) {
                        // Apply color for keywords
                        for (rules.keyword_color) |char| {
                            highlighted_text[highlighted_text_index] = char;
                            highlighted_text_index += 1;
                        }
                        // Copy the keyword
                        for (word) |char| {
                            highlighted_text[highlighted_text_index] = char;
                            highlighted_text_index += 1;
                        }
                        // Reset the color
                        highlighted_text[highlighted_text_index] = '\x1b';
                        highlighted_text_index += 1;
                        highlighted_text[highlighted_text_index] = '[';
                        highlighted_text_index += 1;
                        highlighted_text[highlighted_text_index] = '0';
                        highlighted_text_index += 1;
                        highlighted_text[highlighted_text_index] = 'm';
                        highlighted_text_index += 1;
                        i = j;
                        keyword_found = true;
                        break;
                    }
                }
            }
            if (!keyword_found) {
                highlighted_text[highlighted_text_index] = text[i];
                highlighted_text_index += 1;
                i += 1;
            }
        }
    }

    const new_size = highlighted_text_index;
    var new_ptr = try allocator.realloc(highlighted_text, new_size);
    return new_ptr[0..new_size];
}
pub fn startIncrementalSearch(allocator: std.mem.Allocator) !void {
    search_query = ArrayList(u8).init(allocator);
    search_index = 0;
    search_direction = 1;
    is_searching = true;
    try setStatusMessage("Search: ", 0);
    try refreshscreen(allocator);

    // Enter a loop to handle search input
    while (is_searching) {
        try refreshscreen(allocator);
        const c = try readkeypress();
        try handleSearchInput(c, allocator);
    }
}
pub fn handleSearchInput(c: u8, allocator: std.mem.Allocator) !void {
    switch (c) {
        '\x1b' => { // Escape
            is_searching = false;
            search_query.deinit();
            try setStatusMessage("", 0);
        },
        127 => { // Backspace
            if (search_query.items.len > 0) {
                _ = search_query.pop();
                try searchInRows();
                try updateSearchStatusMessage(allocator);
            }
        },
        '\r' => { // Enter
            if (search_query.items.len > 0) {
                try findNext();
            } else {
                is_searching = false;
            }
        },
        @intFromEnum(keys.ARROW_UP), @intFromEnum(keys.ARROW_DOWN) => {
            search_direction = if (c == @intFromEnum(keys.ARROW_UP)) -1 else 1;
            try findNext();
        },
        else => {
            if (c >= 32 and c < 127) { // Printable ASCII
                try search_query.append(c);
                search_index = 0;
                search_direction = 1;
                try updateSearchStatusMessage(allocator);
                try searchInRows();
            }
        },
    }
    try updateSearchStatusMessage(allocator);
}
pub fn updateSearchStatusMessage(allocator: std.mem.Allocator) !void {
    try setStatusMessage(try std.fmt.allocPrint(allocator, "Search: {s}", .{search_query.items}), 1);
}
pub fn searchInRows() !void {
    if (search_query.items.len == 0) return;

    const start_row = editorvar.cy;
    var start_col = editorvar.cx + 1;
    var found = false;

    for (0..editorvar.numrows) |i| {
        const row_index = (start_row + i) % editorvar.numrows;
        const row = &editorvar.row[row_index];
        if (mem.indexOfPos(u8, row.chars.?[0..row.size], start_col, search_query.items)) |index| {
            editorvar.cy = @intCast(row_index);
            editorvar.cx = @intCast(index);
            editorvar.cx += 1;
            editorvar.rowoffset = editorvar.numrows;
            found = true;
            break;
        }
        start_col = 0;
    }

    if (!found) {
        editorvar.cy = start_row;
        if (start_col > 0) {
            editorvar.cx = start_col - 1;
        } else {
            editorvar.cx = 0;
        }
    }
}

pub fn findNext() !void {
    const original_cy = editorvar.cy;
    const original_cx = editorvar.cx;
    editorvar.cy = @intCast(@mod(@as(i32, editorvar.cy) + search_direction + @as(i32, editorvar.numrows), @as(i32, editorvar.numrows)));
    if (search_direction == 1) {
        editorvar.cx = 0;
    } else {
        editorvar.cx = editorvar.row[editorvar.cy].size;
    }

    try searchInRows();

    if (editorvar.cy == original_cy and editorvar.cx == original_cx) {
        try setStatusMessage("No more occurrences found", 2);
    }
}

//TODO: create newfile newdir with a
//if search is dir display in format, display subfiles and folders
//BUG: . firstcharacter is ignored
pub fn opendirectory(allocator: std.mem.Allocator) !void {
    var buff = try abuf.init();
    defer buff.deinit(allocator);
    try abappend(&buff, allocator, "\x1b[2J");
    try abappend(&buff, allocator, "\x1b[H");
    const path = try std.fs.cwd().realpathAlloc(allocator, ".");
    const iterablepath = try std.fs.openDirAbsolute(path, .{ .iterate = true });
    var filebuffer = try iterablepath.walk(allocator);
    defer filebuffer.deinit();
    const stdout = std.io.getStdOut().writer();
    var matchedfiles = std.ArrayList([]const u8).init(allocator);
    var displayfiles = std.ArrayList([]const u8).init(allocator);

    defer {
        for (matchedfiles.items) |item| {
            allocator.free(item);
        }
        matchedfiles.deinit();
    }
    defer {
        for (displayfiles.items) |item| {
            allocator.free(item);
        }
        displayfiles.deinit();
    }

    while (try filebuffer.next()) |nextfile| {
        if (std.mem.indexOfScalar(u8, nextfile.path, '.')) |dot_index| {
            const after_dot = nextfile.path[dot_index + 1 ..];
            if (std.mem.indexOfScalar(u8, after_dot, std.fs.path.sep)) |_| {
                // This is a hidden file or directory, or inside a hidden directory
                continue;
            }
        }
        if (std.mem.indexOfScalar(u8, nextfile.path, '.')) |dot_index| {
            const after_dot = nextfile.path[dot_index + 1 ..];
            if (std.mem.indexOfScalar(u8, after_dot, std.fs.path.sep)) |_| {
                // This is a hidden file or directory, or inside a hidden directory
                continue;
            }
        }
        var filepath: []const u8 = undefined;
        if (std.fs.path.dirname(nextfile.path)) |dir| {
            filepath = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir, nextfile.basename });
        } else {
            filepath = try std.fmt.allocPrint(allocator, "{s}", .{nextfile.basename});
        }
        const relpath = try std.fs.path.relative(allocator, path, filepath);
        if (nextfile.kind == .file) {
            try matchedfiles.append(filepath);
            try matchedfiles.append(relpath);
            const filedepth = std.mem.count(u8, nextfile.path, "/");
            const indentlevel = if (filedepth == 0) 0 else filedepth;
            if (std.mem.startsWith(u8, nextfile.basename, ".")) {
                continue;
            } else if (nextfile.basename.len > 20) {
                if (filedepth > 0) {
                    const indent = try allocator.alloc(u8, indentlevel);
                    @memset(indent, ' ');
                    const opstring = try std.fmt.allocPrint(allocator, "{s}{s}", .{ indent, nextfile.basename[0..20] });
                    try abappend(
                        &buff,
                        allocator,
                        opstring,
                    );
                } else {
                    const opstring = try std.fmt.allocPrint(allocator, "{s}", .{nextfile.basename[0..20]});
                    try abappend(
                        &buff,
                        allocator,
                        opstring,
                    );
                }
                try abappend(&buff, allocator, "\n \x1b[0G");
            } else {
                if (filedepth > 0) {
                    const indent = try allocator.alloc(u8, filedepth);
                    @memset(indent, ' ');
                    const opstring = try std.fmt.allocPrint(allocator, "{s}{s}", .{ indent, nextfile.basename });
                    try abappend(
                        &buff,
                        allocator,
                        opstring,
                    );
                } else {
                    const opstring = try std.fmt.allocPrint(allocator, "{s}", .{nextfile.basename});
                    try abappend(
                        &buff,
                        allocator,
                        opstring,
                    );
                }
                try abappend(&buff, allocator, "\n \x1b[0G");
            }
        } else if (nextfile.kind == .directory) {
            try matchedfiles.append(filepath);
            try matchedfiles.append(relpath);
            const filedepth = std.mem.count(u8, nextfile.path, "/");
            const indentlevel = if (filedepth == 0) 0 else filedepth;
            if (nextfile.basename.len > 20) {
                const filename = try allocator.dupe(u8, nextfile.basename[0..20]);
                if (indentlevel > 0) {
                    const indent = try allocator.alloc(u8, indentlevel);
                    @memset(indent, ' ');
                    const opstring = try std.fmt.allocPrint(allocator, "{s}L{s}", .{ indent, nextfile.basename[0..20] });
                    try abappend(&buff, allocator, "\x1b[34m");
                    try abappend(
                        &buff,
                        allocator,
                        opstring,
                    );

                    try abappend(&buff, allocator, "\x1b[0m");
                    try abappend(&buff, allocator, "\n \x1b[0G");
                } else {
                    const opstring = try std.fmt.allocPrint(allocator, "L{s}", .{nextfile.basename[0..20]});
                    try matchedfiles.append(filename);
                    try abappend(&buff, allocator, "\x1b[34m");
                    try abappend(
                        &buff,
                        allocator,
                        opstring,
                    );

                    try abappend(&buff, allocator, "\x1b[0m");
                    try abappend(&buff, allocator, "\n \x1b[0G");
                }
            } else {
                if (indentlevel > 0) {
                    const indent = try allocator.alloc(u8, indentlevel);
                    @memset(indent, ' ');

                    const opstring = try std.fmt.allocPrint(allocator, "{s}L{s}", .{ indent, nextfile.basename });

                    try abappend(&buff, allocator, "\x1b[34m");
                    try abappend(
                        &buff,
                        allocator,
                        opstring,
                    );

                    try abappend(&buff, allocator, "\x1b[0m");
                    try abappend(&buff, allocator, "\n \x1b[0G");
                } else {
                    const opstring = try std.fmt.allocPrint(allocator, "L{s}", .{nextfile.basename});

                    try abappend(&buff, allocator, "\x1b[34m");
                    try abappend(
                        &buff,
                        allocator,
                        opstring,
                    );

                    try abappend(&buff, allocator, "\x1b[0m");
                    try abappend(&buff, allocator, "\n \x1b[0G");
                }
            }
        } else {
            break;
        }
    }
    _ = try stdout.write(buff.b.?[0..buff.len]);
    buff.len = 0;
    var filesearch = std.ArrayList(u8).init(allocator);
    defer filesearch.deinit();
    var input = try readkeypress();
    if (input == 'i') {
        while (true) {
            buff.len = 0;
            displayfiles.clearRetainingCapacity();
            for (matchedfiles.items[1..]) |file| {
                if (filesearch.items.len == 0 or std.mem.indexOf(u8, file, filesearch.items) != null) {
                    try displayfiles.insert(displayfiles.items.len, file);
                }
            }
            var unique = std.StringHashMap(void).init(allocator);
            defer unique.deinit();
            for (displayfiles.items) |value| {
                try unique.put(value, {});
            }
            var it = unique.keyIterator();
            var dir_list = std.ArrayList([]const u8).init(allocator);

            while (it.next()) |file| {
                try dir_list.append(file.*);
            }
            const compareStrings = struct {
                fn compare(_: void, lhs: []const u8, rhs: []const u8) bool {
                    const lhs_slashes = std.mem.count(u8, lhs, "/");
                    const rhs_slashes = std.mem.count(u8, rhs, "/");
                    return lhs_slashes > rhs_slashes;
                }
            }.compare;
            std.mem.sort([]const u8, dir_list.items, {}, compareStrings);
            while (dir_list.popOrNull()) |file| {
                if (std.mem.indexOf(u8, file, ".") != null) {
                    const filestring = try std.fmt.allocPrint(allocator, "\n \x1b[0G {s} ", .{file});
                    try abappend(&buff, allocator, filestring);
                } else {
                    const filestring = try std.fmt.allocPrint(allocator, "\n \x1b[0G {s} ", .{file});
                    try abappend(&buff, allocator, "\x1b[34m");
                    try abappend(&buff, allocator, filestring);
                    try abappend(&buff, allocator, "\x1b[0m");
                }
            }
            input = try readkeypress();
            switch (input) {
                '\r' => {
                    break;
                },
                127 => {
                    if (filesearch.items.len > 0) {
                        _ = filesearch.pop();
                        const searchstring = try std.fmt.allocPrint(allocator, "\x1b[{d}G \n \x1b[0G Search: {s}", .{ editorvar.numrows - 1, filesearch.items });
                        try abappend(&buff, allocator, searchstring);
                    } else {
                        continue;
                    }
                },
                ' ' => {
                    var newfile = std.ArrayList(u8).init(allocator);
                    defer newfile.deinit();
                    var temp: u8 = undefined;
                    while (temp != '\r') {
                        temp = try readkeypress();
                        switch (temp) {
                            '\r' => {
                                if (newfile.getLast() == '/') {
                                    const newdir = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ path, newfile.items });
                                    try std.fs.makeDirAbsolute(newdir);
                                    const newdir_free = try allocator.dupe(u8, newdir);
                                    try abappend(&buff, allocator, "\n \x1b[0G");
                                    try abappend(&buff, allocator, newdir_free);
                                    allocator.free(newdir);
                                } else {
                                    const newfilestring = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ path, newfile.items });
                                    _ = try std.fs.createFileAbsolute(newfilestring, .{});
                                    const newfilestring_free = try allocator.dupe(u8, newfilestring);
                                    try abappend(&buff, allocator, "\n \x1b[0G");
                                    try abappend(&buff, allocator, newfilestring_free);
                                    allocator.free(newfilestring);
                                }
                                break;
                            },
                            127 => {
                                if (newfile.items.len > 0) {
                                    _ = newfile.pop();
                                    const newstring = try std.fmt.allocPrint(allocator, "\x1b[{d}G \n \x1b[0G Search: {s}", .{ editorvar.numrows - 1, newfile.items });
                                    try abappend(&buff, allocator, newstring);
                                } else {
                                    continue;
                                }
                            },
                            else => {
                                try newfile.append(temp);
                            },
                        }
                        std.debug.print("\x1b[2J", .{});
                        _ = try stdout.write(buff.b.?[0..buff.len]);
                    }
                },
                else => {
                    try filesearch.append(input);
                    const searchstring = try std.fmt.allocPrint(allocator, "\x1b[{d}G \n \x1b[0G Search: {s}", .{ editorvar.numrows - 1, filesearch.items });
                    try abappend(&buff, allocator, searchstring);
                },
            }
            std.debug.print("\x1b[2J", .{});
            _ = try stdout.write(buff.b.?[0..buff.len]);
        }
    }
    const searchstring: []const u8 = try std.fmt.allocPrint(allocator, "{s}", .{filesearch.items});
    var flag: u8 = 0;
    for (displayfiles.items) |value| {
        if (std.mem.eql(u8, searchstring, value)) {
            flag = 0;
            const fullpath = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ path, searchstring });
            const filetype = try std.fs.cwd().statFile(fullpath);
            switch (filetype.kind) {
                .directory => {
                    var dir = try std.fs.cwd().openDir(fullpath, .{ .iterate = true });
                    defer dir.close();
                    try dir.setAsCwd();
                    try opendirectory(allocator);
                },
                .file => {
                    for (0..editorvar.numrows) |_| {
                        if (editorvar.row[0].chars) |chars| {
                            allocator.free(chars);
                        }
                    }
                    allocator.free(editorvar.row);
                    editorvar.row = &[_]erow{};
                    editorvar.numrows = 0;
                    try editoropen(allocator, fullpath);
                    editorvar.dirty = 0;
                    break;
                },
                else => {},
            }
            break;
        } else {
            flag = 1;
            continue;
        }
    }
    try setStatusMessage("No file found", 3);
}
pub fn readkeypress() !u8 {
    const stdin = std.io.getStdIn().reader();
    var char: u8 = 0;
    char = try stdin.readByte();
    if (char == '\x1b') {
        var sequence: [3]u8 = undefined;
        _ = try stdin.readAtLeast(&sequence, 2);
        if (sequence[0] == '[') {
            switch (sequence[1]) {
                'A' => {
                    return @intFromEnum(keys.ARROW_UP);
                },
                'B' => {
                    return @intFromEnum(keys.ARROW_DOWN);
                },
                'C' => {
                    return @intFromEnum(keys.ARROW_RIGHT);
                },
                'D' => {
                    return @intFromEnum(keys.ARROW_LEFT);
                },
                '5' => {
                    return @intFromEnum(keys.PAGE_UP);
                },
                '6' => {
                    return @intFromEnum(keys.PAGE_DOWN);
                },
                'H' => {
                    return @intFromEnum(keys.HOME_KEY);
                },
                'F' => {
                    return @intFromEnum(keys.END_KEY);
                },
                '3' => {
                    if (sequence[2] == '~') {
                        return @intFromEnum(keys.DEL_KEY);
                    }
                },
                else => {},
            }
        }
    }
    return char;
}

pub fn processkeypress(allocator: std.mem.Allocator) !void {
    const c = try readkeypress();
    const stdout = std.io.getStdOut().writer();
    if (c >= 32 and c <= 126) { // normal character
        try insertchar(c, allocator);
    } else {
        if (c == ctrl_key('q') or c == ctrl_key('Q')) {
            if (editorvar.dirty > 0 and quit_times > 0) {
                try setStatusMessage("Unsaved changes, press ctrl+Q again to quit", 3);
                quit_times -= 1;
                return;
            }
            _ = try stdout.write("\x1b[2J");
            _ = try stdout.write("\x1b[H");
            c_header.exit(0);
        } else if (c == ctrl_key('s')) {
            if (std.mem.eql(u8, "Untitled", editorvar.filename)) {
                var buffer: [256]u8 = undefined;
                var i: usize = 0;
                try setStatusMessage("Save as: ", 0);
                while (i < buffer.len) {
                    try refreshscreen(allocator);
                    const key = try readkeypress();
                    if (key == '\r') { // Enter key
                        break;
                    } else if (key == 127) { // Backspace
                        if (i > 0) {
                            i -= 1;
                            buffer[i] = 0;
                            try setStatusMessage(std.fmt.bufPrint(&buffer, "Save as: {s}", .{buffer[0..i]}) catch unreachable, 0);
                        }
                    } else if (i < buffer.len - 1 and key >= 32 and key <= 126) {
                        buffer[i] = key;
                        i += 1;
                        try setStatusMessage(std.fmt.bufPrint(&buffer, "Save as: {s}", .{buffer[0..i]}) catch unreachable, 0);
                    }
                }
                if (i > 0) {
                    var cwd_buffer: [std.fs.MAX_PATH_BYTES]u8 = undefined;
                    const cwd = try std.fs.cwd().realpath("", &cwd_buffer);
                    const new_filename = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ cwd, buffer[0..i] });
                    defer allocator.free(new_filename);
                    editorvar.filename = try allocator.dupe(u8, new_filename);
                } else {
                    try setStatusMessage("Save aborted", 3);
                    return;
                }
            }
            const message = try std.fmt.allocPrint(allocator, "Saving to file: {s}", .{editorvar.filename});
            defer allocator.free(message);
            try writefile(allocator);
        } else if (c == ctrl_key('f') or c == ctrl_key('F')) {
            try startIncrementalSearch(allocator);
        } else if (c == @intFromEnum(keys.ARROW_UP)) {
            try movecursor('A');
        } else if (c == @intFromEnum(keys.ARROW_DOWN)) {
            try movecursor('B');
        } else if (c == @intFromEnum(keys.ARROW_LEFT)) {
            try movecursor('D');
        } else if (c == @intFromEnum(keys.ARROW_RIGHT)) {
            try movecursor('C');
        } else if (c == @intFromEnum(keys.DEL_KEY)) {
            try movecursor('C');
            try deletechar(allocator);
        } else if (c == @intFromEnum(keys.BACKSPACE)) {
            try deletechar(allocator);
        } else if (c == @intFromEnum(keys.HOME_KEY)) {
            editorvar.cx = 0;
        } else if (c == @intFromEnum(keys.END_KEY)) {
            editorvar.cx = editorvar.row[editorvar.cy].size;
        } else if (c == @intFromEnum(keys.PAGE_UP)) {
            editorvar.cy = if (editorvar.cy > editorvar.rows) editorvar.cy - editorvar.rows else 0;
        } else if (c == @intFromEnum(keys.PAGE_DOWN)) {
            editorvar.cy = if (editorvar.cy + editorvar.rows <= editorvar.numrows) editorvar.cy + editorvar.rows else editorvar.cy;
        } else if (c == '\t') {
            try insertchar(' ', allocator);
            try insertchar(' ', allocator);
            try insertchar(' ', allocator);
            try insertchar(' ', allocator);
        } else if (c == '\r') {
            try enter(allocator);
        } else if (c == ctrl_key('z')) {
            try opendirectory(allocator);
        }
    }
}

pub fn appendrow(allocator: std.mem.Allocator, at: u16) !void {
    if (at == 0 or at >= editorvar.numrows) {
        return;
    }

    var current_row = &editorvar.row[at];
    var prev_row = &editorvar.row[at - 1];

    const new_size = prev_row.size + current_row.size;
    prev_row.chars = try allocator.realloc(prev_row.chars.?, new_size);

    @memcpy(prev_row.chars.?[prev_row.size..new_size], current_row.chars.?[0..current_row.size]);
    prev_row.size = new_size;

    // Remove the current row
    for (at..editorvar.numrows - 1) |i| {
        editorvar.row[i] = editorvar.row[i + 1];
    }
    editorvar.numrows -= 1;
    editorvar.row = try allocator.realloc(editorvar.row, editorvar.numrows);

    editorvar.dirty = 1;
}
pub fn deletecharatrow(row: *erow, at: u16) !void {
    if (at >= row.size) {
        return;
    }
    std.mem.copyForwards(u8, row.chars.?[at..row.size], row.chars.?[at + 1 .. row.size]);
    row.size -= 1;
}
pub fn deletechar(allocator: std.mem.Allocator) !void {
    if (editorvar.cy == editorvar.numrows) {
        return;
    }
    const row = &editorvar.row[editorvar.cy];
    if (editorvar.cx > 0) {
        try deletecharatrow(row, editorvar.cx - 1);
        editorvar.cx -= 1;
    } else if (editorvar.cy > 0) {
        // We're at the beginning of a line, so append this row to the previous one
        editorvar.cx = editorvar.row[editorvar.cy - 1].size; // Move cursor to end of previous line
        try appendrow(allocator, editorvar.cy);
        editorvar.cy -= 1;
    }
}
pub fn removeBlankLines(allocator: std.mem.Allocator) !void {
    var i: usize = 0;
    while (i < editorvar.numrows) {
        if (editorvar.row[i].size == 0) {
            // This is a blank line, remove it
            for (i..editorvar.numrows - 1) |j| {
                editorvar.row[j] = editorvar.row[j + 1];
            }
            editorvar.numrows -= 1;
            editorvar.row = try allocator.realloc(editorvar.row, editorvar.numrows);
            editorvar.dirty = 1;
        } else {
            i += 1;
        }
    }
}
pub fn insertcharatrow(row: *erow, at: u16, char: u8, allocator: std.mem.Allocator) !void {
    const new_size = row.size + 1;
    if (row.chars) |chars| {
        row.chars = try allocator.realloc(chars, new_size);
    } else {
        row.chars = try allocator.alloc(u8, new_size);
    }

    // If the row was previously empty, just set the character
    if (row.size == 0) {
        row.chars.?[0] = char;
    } else {
        // Move the existing characters to make room for the new one
        if (at < row.size) {
            std.mem.copyBackwards(u8, row.chars.?[at + 1 .. new_size], row.chars.?[at .. new_size - 1]);
        }
        row.chars.?[at] = char;
    }

    row.size = new_size;
    editorvar.dirty = 1;
}
pub fn insertchar(c: u8, allocator: std.mem.Allocator) !void {
    if (editorvar.cy == editorvar.numrows) {
        try insertRow(editorvar.cy, "", allocator);
    }

    const row = &editorvar.row[editorvar.cy];
    try insertcharatrow(row, editorvar.cx, c, allocator);
    editorvar.cx += 1;
    editorvar.dirty = 1;
}

pub fn editorprompt(prompt: []const u8, allocator: std.mem.Allocator) !*u8 {
    var bufsize: u16 = 128;
    var buff = try allocator.alloc(u8, bufsize);
    while (true) {
        try setStatusMessage(prompt, 3);
        const c: u8 = try readkeypress();
        if (c == '\r') {
            if (buff.len > 0) {
                try setStatusMessage("", 3);
            }
        } else if (!(ctrl_key(c) != 0) and c < 128) {
            if (buff.len == bufsize - 1) {
                bufsize *= 2;
                buff = try allocator.realloc(buff, bufsize);
            }
            const at = buff.len + 1;
            buff[at] = c;
        }
    }
}

pub fn movecursor(char: u8) !void {
    var temperow: ?erow = if (editorvar.cy < editorvar.numrows) editorvar.row[editorvar.cy] else null;

    switch (char) {
        'A' => {
            if (editorvar.cy != 0) {
                editorvar.cy -= 1;
            }
        },
        'B' => {
            if (editorvar.cy < editorvar.numrows) {
                editorvar.cy += 1;
            }
        },
        'D' => {
            if (editorvar.cx != 0) {
                editorvar.cx -= 1;
            } else {
                if (editorvar.cy > 0) {
                    editorvar.cy -= 1;
                    editorvar.cx = editorvar.row[editorvar.cy].size;
                }
            }
        },
        'C' => {
            if (temperow) |row| {
                if (editorvar.cx < row.size) {
                    editorvar.cx += 1;
                } else {
                    if (editorvar.cx == row.size) {
                        editorvar.cy += 1;
                        editorvar.cx = 0;
                    }
                }
            }
        },
        else => {},
    }
    temperow = if (editorvar.cy < editorvar.numrows) editorvar.row[editorvar.cy] else null;
    const rowlen: u16 = if (temperow) |row| row.size else 0;
    if (editorvar.cx > rowlen) {
        editorvar.cx = rowlen;
    }
}

pub fn drawstatusbar(buf: *abuf, allocator: std.mem.Allocator) !void {
    try abappend(buf, allocator, "\x1b[48;2;000;000;000m\x1b[97m");

    var status: [80]u8 = undefined;
    const fileinfo = try std.fmt.bufPrint(&status, "{s} - Line {d}", .{ editorvar.filename, editorvar.cy + 1 });
    var combined: [256]u8 = undefined;
    var i: usize = 0;

    const current_time = std.time.timestamp();
    const message = if (current_time < statusmsg.time)
        statusmsg.message[0 .. std.mem.indexOfScalar(u8, &statusmsg.message, 0) orelse statusmsg.message.len]
    else
        "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find";

    const modified_msg = if (editorvar.dirty == 1) "(Modified)" else "";
    const left_padding = (editorvar.cols - message.len) / 2;

    if (editorvar.dirty == 1) {
        const mod_msg_len = modified_msg.len;
        @memcpy(combined[i .. i + mod_msg_len], modified_msg);
        i += mod_msg_len;
        combined[i] = ' ';
        i += 1;
    }

    while (i < left_padding) : (i += 1) {
        combined[i] = ' ';
    }

    @memcpy(combined[i .. i + message.len], message);
    i += message.len;

    while (i < editorvar.cols - fileinfo.len) : (i += 1) {
        combined[i] = ' ';
    }

    @memcpy(combined[i .. i + fileinfo.len], fileinfo);
    i += fileinfo.len;

    const len = @min(i, editorvar.cols);
    try abappend(buf, allocator, combined[0..len]);
    try abappend(buf, allocator, "\x1b[m");
}
//output function
pub fn drawrows(buf: *abuf, allocator: std.mem.Allocator) !void {
    const line_number_width = 4; // Width of the line number bar

    var y: usize = 0;
    while (y < editorvar.rows) : (y += 1) {
        const filerow: usize = y + editorvar.rowoffset;

        // Draw the line number for the current row
        try abappend(buf, allocator, "\x1b[K"); // Clear the line
        if (filerow < editorvar.numrows) {
            var line_num_buf: [5]u8 = undefined; // 4 digits + null terminator
            const line_num = try std.fmt.bufPrint(&line_num_buf, "{d:4}", .{filerow + 1});
            try abappend(buf, allocator, "\x1b[90m"); // Invert colors for line number
            try abappend(buf, allocator, line_num);
            try abappend(buf, allocator, "\x1b[m"); // Reset colors
        } else {
            try abappend(buf, allocator, "    ");
        }

        // Now draw the actual content of the row
        if (filerow >= editorvar.numrows) {
            if (editorvar.numrows == 0 and y == editorvar.rows / 3) {
                const welcome: []const u8 = "Hyperion -- version 0.1.0";
                var padding: u64 = (editorvar.cols - welcome.len - line_number_width - 3) / 2;
                try setStatusMessage("Welcome!", 3);
                if (padding > 0) {
                    try abappend(buf, allocator, " ");
                    padding -= 1;
                }
                while (padding > 0) : (padding -= 1) {
                    try abappend(buf, allocator, " ");
                }
                try abappend(buf, allocator, welcome);
            } else {
                try abappend(buf, allocator, " ");
            }
        } else {
            var scrlen: usize = undefined;
            if (editorvar.row[filerow].size > editorvar.coloffset) {
                scrlen = editorvar.row[filerow].size - editorvar.coloffset;
            } else {
                scrlen = 0;
            }
            if (scrlen > editorvar.cols - line_number_width - 3) {
                scrlen = editorvar.cols - line_number_width - 3;
            }
            if (editorvar.row[filerow].chars) |chars| {
                if (chars.len > scrlen) {
                    try enter(allocator);
                } else {
                    const start = editorvar.coloffset;
                    const end = editorvar.coloffset + scrlen;
                    const highlighted_text = try highlightText(chars[start..end], 0, allocator);
                    defer allocator.free(highlighted_text);
                    try abappend(buf, allocator, highlighted_text);
                }
            }
        }

        try abappend(buf, allocator, "\x1b[K");
        try abappend(buf, allocator, "\r\n");
    }
}

pub fn scroll() !void {
    if (editorvar.cy < editorvar.rowoffset) {
        editorvar.rowoffset = editorvar.cy;
    }
    if (editorvar.cy >= editorvar.rowoffset + editorvar.rows) {
        editorvar.rowoffset = editorvar.cy - editorvar.rows + 1;
    }
    if (editorvar.cx < editorvar.coloffset) {
        editorvar.coloffset = editorvar.cx;
    }
    if (editorvar.cx >= editorvar.coloffset + editorvar.cols) {
        editorvar.coloffset = editorvar.cx - editorvar.cols + 1;
    }
}

pub fn errorbuffer(allocator: std.mem.Allocator, message: []const u8) !void {
    var buf = try abuf.init();
    defer buf.deinit(allocator);

    // Clear the screen and move cursor to top-left
    try abappend(&buf, allocator, "\x1b[2J");
    try abappend(&buf, allocator, "\x1b[H");
    try abappend(&buf, allocator, "\x1b[31m");
    try abappend(&buf, allocator, message);
    try abappend(&buf, allocator, "\x1b[0m"); // Reset text color
    try abappend(&buf, allocator, "\x1b[?25l"); // Hide cursor

    // Write the buffer to stdout
    const stdout = std.io.getStdOut().writer();
    _ = try stdout.write(buf.b.?[0..buf.len]);

    // Wait for a keypress before returning
    _ = try readkeypress();

    // Clear the screen and move cursor back to top-left
    // Show cursor
    _ = try stdout.write(buf.b.?[0..buf.len]);
}

pub fn refreshscreen(allocator: std.mem.Allocator) !void {
    try scroll();
    var buf: abuf = try abuf.init();
    defer buf.deinit(allocator);

    try abappend(&buf, allocator, "\x1b[?25l");
    try abappend(&buf, allocator, "\x1b[H");

    try drawrows(&buf, allocator);
    try drawstatusbar(&buf, allocator);

    if (is_searching) {
        try updateSearchStatusMessage(allocator);
    }

    var charbuff: [32]u8 = undefined;
    const line_number_width = 4; // Width of the line number bar
    const string = try std.fmt.bufPrint(&charbuff, "\x1b[{};{}H", .{
        (editorvar.cy - editorvar.rowoffset) + 1,
        (editorvar.cx - editorvar.coloffset) + line_number_width + 1,
    });
    try abappend(&buf, allocator, string);

    try abappend(&buf, allocator, "\x1b[?25h");
    const stdout = std.io.getStdOut().writer();
    _ = try stdout.write(buf.b.?[0..buf.len]);
}

pub fn setStatusMessage(msg: []const u8, timeout_secs: i64) !void {
    const timestamp = std.time.timestamp();
    @memcpy(statusmsg.message[0..msg.len], msg);
    statusmsg.message[msg.len] = 0; // Null-terminate the string
    statusmsg.time = timestamp + timeout_secs;
}
// Main function

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Access command-line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var filename: ?[]const u8 = null;

    if (args.len > 1) {
        filename = args[1]; // Use the first argument as the file name
    }

    try rawmode();
    try initeditor(allocator);

    if (filename) |file| {
        try editoropen(allocator, file); // Open the provided file
    } else {
        try editoropen(allocator, "Untitled");
    }

    try setStatusMessage("Hyperion - 0.1.0", 2);

    while (true) {
        try refreshscreen(allocator);
        const current_time = std.time.timestamp();
        if (current_time >= statusmsg.time and last_update_time != current_time) {
            statusmsg.time = 0; // Clear the message
            last_update_time = current_time;
        }
        try processkeypress(allocator);
    }
}
