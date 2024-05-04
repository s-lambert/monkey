const std = @import("std");
const lexer = @import("./lexer.zig");

const Expression = union(enum) {
    integer: []const u8,
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
    },
    ret: struct {
        value: ?Expression,
    },
    empty,
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
            else => null,
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

        while (self.peek_token != .semicolon or self.peek_token == .eof) {
            self.next_token();
        }

        var expression = self.parse_expression();

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

        while (self.peek_token != .semicolon or self.peek_token == .eof) {
            self.next_token();
        }

        var expression = self.parse_expression();

        return .{ .ret = .{
            .value = expression,
        } };
    }

    fn parse_expression(self: *Self) ?Expression {
        return switch (self.curr_token) {
            .int => |value| .{ .integer = value },
            else => null,
        };
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
    try std.testing.expectEqualDeep(
        Expression{ .integer = "5" },
        program.statements.items[0].let.value,
    );
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
    try std.testing.expectEqualDeep(
        Expression{ .integer = "10" },
        program.statements.items[0].ret.value.?,
    );
}