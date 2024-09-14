// See the README.md of zig-lsp-codegen on how to add the "lsp" module to your project.

const std = @import("std");
const lsp = @import("lsp-codegen");
const HandledRequestParams = union(enum) {
    initialize: lsp.types.InitializeParams,
    // add more requests here
    shutdown,
    other: lsp.MethodWithParams,
};

const HandledNotificationParams = union(enum) {
    initialized: lsp.types.InitializedParams,
    @"textDocument/didOpen": lsp.types.DidOpenTextDocumentParams,
    @"textDocument/publishDiagnostics": lsp.types.PublishDiagnosticsParams,
    // add more notifications here
    exit,
    other: lsp.MethodWithParams,
};

const Message = lsp.Message(.{
    .RequestParams = HandledRequestParams,
    .NotificationParams = HandledNotificationParams,
});

fn sendMessage(allocator: std.mem.Allocator, transport: lsp.AnyTransport, msg: Message) !void {
    const json_message = try std.json.stringifyAlloc(allocator, msg, .{});
    defer allocator.free(json_message);

    try transport.writeJsonMessage(json_message);
}

fn receiveMessage(allocator: std.mem.Allocator, transport: lsp.AnyTransport) !std.json.Parsed(Message) {
    const json_message = try transport.readJsonMessage(allocator);
    defer allocator.free(json_message);
    return try Message.parseFromSlice(allocator, json_message, .{});
}
fn receiveDiagnosticsMessage(allocator: std.mem.Allocator, transport: lsp.AnyTransport) ![]const u8 {
    const json_message = try transport.readJsonMessage(allocator);
    return json_message;
}

pub fn main() !void {
    var general_purpose_allocator: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = general_purpose_allocator.deinit();

    const gpa = general_purpose_allocator.allocator();

    var arg_it = try std.process.argsWithAllocator(gpa);
    defer arg_it.deinit();

    _ = arg_it.skip(); // skip self exe

    // The first argument is the path to the Server (ZLS) executable.
    const child_process_exe = try gpa.dupe(u8, arg_it.next() orelse @panic("expected first argument!"));
    defer gpa.free(child_process_exe);

    // The second argument is the path to the file that should be checked for errors.
    const input_file_path = try gpa.dupe(u8, arg_it.next() orelse @panic("expected second argument!"));
    defer gpa.free(input_file_path);

    // read the file from disk
    const file_source = try std.fs.cwd().readFileAlloc(gpa, input_file_path, std.math.maxInt(u32));
    defer gpa.free(file_source);

    var child = std.process.Child.init(&.{child_process_exe}, gpa);
    child.stdin_behavior = .Pipe;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe; // Set to `.Inherit` to view stderr

    // Start the Server (ZLS)
    try child.spawn();

    std.time.sleep(100 * std.time.ns_per_ms); // Sleep for 100ms

    var transport = lsp.TransportOverStdio.init(child.stdout.?, child.stdin.?);
    // >>> send initialize request to the Server (ZLS)
    try sendMessage(gpa, transport.any(), .{
        .request = .{
            .id = .{ .number = 1 },
            .params = HandledRequestParams{
                .initialize = .{
                    .capabilities = .{
                        .textDocument = .{
                            // This needs to be set so that the Server (ZLS) knows that we support diagnostics
                            .publishDiagnostics = .{},
                        },
                    },
                },
            },
        },
    });
    std.time.sleep(100 * std.time.ns_per_ms);
    // <<< receive initialize response from the Server (ZLS)
    const initialize_response = try receiveMessage(gpa, transport.any());
    defer initialize_response.deinit();

    // Assert that we receive a response to the 'initialize' request.
    // This isn't guaranteed true but let's just keep it simple.
    std.debug.assert(initialize_response.value.response.id.?.eql(.{ .number = 1 }));

    // >>> send initialized notification
    try sendMessage(gpa, transport.any(), .{ .notification = .{ .params = .initialized } });
    const uri = try std.fmt.allocPrint(gpa, "{}", .{std.Uri{
        .scheme = "file",
        .path = .{ .raw = input_file_path },
    }});
    defer gpa.free(uri);

    // >>> send `textDocument/didOpen` request
    try sendMessage(gpa, transport.any(), .{ .notification = .{
        .params = .{ .@"textDocument/didOpen" = lsp.types.DidOpenTextDocumentParams{
            .textDocument = .{
                .uri = uri,
                .languageId = "zig",
                .version = 0,
                .text = file_source,
            },
        } },
    } });

    // <<< receive a messsage which will probably be "textDocument/publishDiagnostics".
    const msg = try receiveDiagnosticsMessage(gpa, transport.any());
    // const json_message = try std.json.stringifyAlloc(gpa, msg, .{});
    const msgfile = try std.fs.openFileAbsolute("/home/covix/Zig/logoutput.txt", .{ .mode = .write_only });
    const val = std.mem.indexOf(u8, msg, "diagnostics");
    if (val) |value| {
        const diagmsg = msg[value + 12 ..];
        try msgfile.seekTo(0);
        try msgfile.writeAll("\x00");
        try msgfile.setEndPos(0);
        try msgfile.writer().writeAll("Diagnostics");
        try msgfile.writer().writeAll(diagmsg);
    }
    defer gpa.free(msg);
    // >>> send the shutdown request
    try sendMessage(gpa, transport.any(), .{ .request = .{
        .id = .{ .number = 2 },
        .params = .shutdown,
    } });

    // >>> send the exit notification
    try sendMessage(gpa, transport.any(), .{ .notification = .{ .params = .exit } });

    _ = try child.wait();
}
