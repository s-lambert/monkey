// curl -O "$(curl https://ziglang.org/download/index.json | jaq -r '.master."x86_64-linux".tarball')"

const std = @import("std");
const lexer = @import("./lexer.zig");

const Precedence = enum(u8) {
    lowest = 0,
    equals = 1,
    lessgreater = 2,
    sum = 3,
    product = 4,
    prefix = 5,
    call = 6,
};

const Expression = union(enum) {
    integer: struct {
        value: u8,
        string: []const u8,
    },
    identifier: []const u8,
    operator: struct {
        // - (.minus) or ! (.bang)
        token: lexer.Token,
        rhs: *Expression,
    },
};

const Identifier = struct {
    token: lexer.Token,
    value: []const u8,
};

const Statement = union(enum) {
    let: struct {
        token: lexer.Token,
        identifier: Identifier,
        value: Expression,

        const Self = @This();

        pub fn print(self: *const Self) void {
            std.log.warn("let {s}", .{self.identifier.value});
        }
    },
    ret: struct {
        value: ?Expression,

        const Self = @This();

        pub fn print(_: *const Self) void {
            std.log.warn("return", .{});
        }
    },
    exp: struct {
        value: Expression,

        const Self = @This();

        pub fn print(_: *const Self) void {
            std.log.warn("expression", .{});
        }
    },

    pub fn print(self: *Statement) void {
        switch (self) {
            inline else => |case| case.print(),
        }
    }
};

const StatementList = std.ArrayList(Statement);
const ParseErrors = std.ArrayList([]const u8);

const Program = struct {
    allocator: std.mem.Allocator,
    statements: StatementList,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .statements = StatementList.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.statements.deinit();
    }

    pub fn print(self: *Self) void {
        for (self.statements.items) |statement| {
            statement.print();
        }
    }
};

const Parser = struct {
    allocator: std.mem.Allocator,
    lex: lexer.Lexer,
    curr_token: lexer.Token,
    peek_token: lexer.Token,
    errors: ParseErrors,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, program_text: []const u8) Self {
        var p: Self = .{
            .allocator = allocator,
            .lex = lexer.Lexer.init(program_text),
            .curr_token = .eof,
            .peek_token = .eof,
            .errors = ParseErrors.init(allocator),
        };

        // Populate curr/peek
        p.next_token();
        p.next_token();

        return p;
    }

    pub fn deinit(self: *Self) void {
        self.errors.deinit();
    }

    pub fn next_token(self: *Self) void {
        self.curr_token = self.peek_token;
        self.peek_token = self.lex.next_token();
    }

    pub fn parse_program(self: *Self) !Program {
        var p = Program.init(self.allocator);

        while (self.curr_token != .eof) {
            if (try self.parse_statement()) |statement| {
                try p.statements.append(statement);
            }
            self.next_token();
        }

        return p;
    }

    fn parse_statement(self: *Self) !?Statement {
        return switch (self.curr_token) {
            .let => try self.parse_let(),
            .ret => try self.parse_return(),
            else => try self.parse_expression_statement(),
        };
    }

    fn parse_let(self: *Self) !?Statement {
        self.next_token();

        var ident: Identifier = .{
            .token = self.curr_token,
            .value = self.curr_token.ident,
        };

        self.next_token();

        if (self.curr_token != .assign) {
            try self.errors.append("let missing assignment");
        }

        while (self.peek_token != .semicolon and self.peek_token != .eof) {
            self.next_token();
        }

        var expression = self.parse_expression(.lowest);

        if (expression) |e| {
            return .{ .let = .{
                .token = .let,
                .identifier = ident,
                .value = e,
            } };
        }

        return null;
    }

    fn parse_return(self: *Self) !?Statement {
        self.next_token();

        while (self.peek_token != .semicolon and self.peek_token != .eof) {
            self.next_token();
        }

        var expression = self.parse_expression(.lowest);

        return .{ .ret = .{
            .value = expression,
        } };
    }

    fn parse_expression_statement(self: *Self) !?Statement {
        if (self.parse_expression(.lowest)) |e| {
            return .{
                .exp = .{
                    .value = e,
                },
            };
        }

        if (self.peek_token == .semicolon or self.peek_token == .eof) {
            self.next_token();
        }

        return null;
    }

    fn parse_expression(self: *Self, _: Precedence) ?Expression {
        if (self.parse_prefix()) |e| {
            return e;
        } else {
            return null;
        }
    }

    fn skip_semicolon(self: *Self) void {
        if (self.peek_token == .semicolon) {
            self.next_token();
        }
    }

    fn parse_prefix(self: *Self) ?Expression {
        return switch (self.curr_token) {
            .ident => |ident| .{ .identifier = ident },
            .int => |int| .{ .integer = .{
                .value = 0,
                .string = int,
            } },
            else => null,
        };
    }

    fn parse_infix(self: *Self, rhs: Expression) void {
        _ = self;
        _ = rhs;
    }
};

test "parse a let statement" {
    var src =
        \\let x = 5;
    ;
    var parser = Parser.init(std.testing.allocator, src);
    defer parser.deinit();
    var program = try parser.parse_program();
    defer program.deinit();
    try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);
    try std.testing.expectEqualDeep(
        Identifier{ .token = .{ .ident = "x" }, .value = "x" },
        program.statements.items[0].let.identifier,
    );
    try std.testing.expectEqualStrings(
        "5",
        program.statements.items[0].let.value.integer.string,
    );
    try std.testing.expectEqual(parser.errors.items.len, 0);
}

test "parse no assign in let statement" {
    var src =
        \\let x 5;
    ;
    var parser = Parser.init(std.testing.allocator, src);
    defer parser.deinit();
    var program = try parser.parse_program();
    defer program.deinit();
    try std.testing.expectEqual(@as(usize, 1), parser.errors.items.len);
}

test "parse return statement" {
    var src =
        \\return 10;
    ;
    var parser = Parser.init(std.testing.allocator, src);
    defer parser.deinit();
    var program = try parser.parse_program();
    defer program.deinit();
    try std.testing.expectEqualStrings(
        "10",
        program.statements.items[0].ret.value.?.integer.string,
    );
    try std.testing.expectEqual(parser.errors.items.len, 0);
}

test "parse a singular integer expression statement" {
    const src = "5;";
    var parser = Parser.init(std.testing.allocator, src);
    defer parser.deinit();
    var program = try parser.parse_program();
    defer program.deinit();
    try std.testing.expectEqualStrings(
        "5",
        program.statements.items[0].exp.value.integer.string,
    );
    try std.testing.expectEqual(parser.errors.items.len, 0);
}

test "parse a singular identifier expression statement" {
    const src = "foo;";
    var parser = Parser.init(std.testing.allocator, src);
    defer parser.deinit();
    var program = try parser.parse_program();
    defer program.deinit();
    try std.testing.expectEqualStrings(
        "foo",
        program.statements.items[0].exp.value.identifier,
    );
    try std.testing.expectEqual(parser.errors.items.len, 0);
}
