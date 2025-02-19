// curl -O "$(curl https://ziglang.org/download/index.json | jaq -r '.master."x86_64-linux".tarball')"

const std = @import("std");
const lexer = @import("./lexer.zig");
const Token = lexer.Token;
const ArrayList = std.ArrayList;
const test_allocator = std.testing.allocator;

const Precedence = enum(u8) {
    lowest = 0, // default, (
    equals = 1, // ==, !=
    lessgreater = 2, // <, >, <=, >=
    sum = 3, // +, -
    product = 4, // *, /
    prefix = 5, // !_, -_
    call = 6,
};

fn token_precedence(t: Token) Precedence {
    return switch (t) {
        Token.equal, Token.not_equal => .equals,
        Token.gt, Token.gte, Token.lt, Token.lte => .lessgreater,
        Token.plus, Token.minus => .sum,
        Token.multiply, Token.divide => .product,
        Token.bang => .prefix,
        else => .lowest,
    };
}

const Expression = union(enum) {
    integer: struct {
        value: u8,
        string: []const u8,

        const Self = @This();

        pub fn print(self: *const Self, w: ArrayList(u8).Writer) void {
            _ = w.write(self.string) catch return;
        }
    },
    identifier: []const u8,
    prefix: struct {
        // - (.minus) or ! (.bang)
        token: Token,
        rhs: ?*Expression,

        const Self = @This();

        pub fn print(self: *const Self, w: ArrayList(u8).Writer) void {
            _ = w.write(@tagName(self.token)) catch return;
            if (self.rhs) |e| {
                e.print(w);
            }
        }
    },
    infix: struct {
        op: Token,
        lhs: ?*Expression,
        rhs: ?*Expression,

        const Self = @This();

        pub fn print(self: *const Self, w: ArrayList(u8).Writer) void {
            _ = w.write("(") catch return;
            if (self.lhs) |e| {
                e.print(w);
            }
            _ = std.fmt.format(w, " {s} ", .{@tagName(self.op)}) catch return;
            if (self.rhs) |e| {
                e.print(w);
            }
            _ = w.write(")") catch return;
        }
    },

    pub fn print(self: Expression, w: ArrayList(u8).Writer) void {
        switch (self) {
            .identifier => |ident| {
                _ = w.write(ident) catch return;
            },
            inline else => |case| case.print(w),
        }
    }
};

const Identifier = struct {
    token: Token,
    value: []const u8,
};

const Statement = union(enum) {
    let: struct {
        token: Token,
        identifier: Identifier,
        value: Expression,

        const Self = @This();

        pub fn print(self: *const Self, w: ArrayList(u8).Writer) void {
            _ = w.write(@tagName(self.token)) catch return;
        }
    },
    ret: struct {
        value: ?Expression,

        const Self = @This();

        pub fn print(self: *const Self, w: ArrayList(u8).Writer) void {
            _ = w.write("return") catch return;
            if (self.value) |value| {
                value.print(w);
            }
        }
    },
    exp: struct {
        value: Expression,

        const Self = @This();

        pub fn print(self: *const Self, w: ArrayList(u8).Writer) void {
            self.value.print(w);
        }
    },

    pub fn print(self: Statement, w: ArrayList(u8).Writer) void {
        switch (self) {
            inline else => |case| case.print(w),
        }
    }
};

const StatementList = std.ArrayList(Statement);
const ParseErrors = std.ArrayList([]const u8);
const Expressions = std.ArrayList(*Expression);

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

    pub fn print(self: *Self, w: ArrayList(u8).Writer) void {
        for (self.statements.items) |*statement| {
            statement.print(w);
        }
    }
};

const Parser = struct {
    allocator: std.mem.Allocator,
    lex: lexer.Lexer,
    curr_token: Token,
    peek_token: Token,
    errors: ParseErrors,
    exprs: Expressions,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, program_text: []const u8) Self {
        var p: Self = .{
            .allocator = allocator,
            .lex = lexer.Lexer.init(program_text),
            .curr_token = .eof,
            .peek_token = .eof,
            .errors = ParseErrors.init(allocator),
            .exprs = Expressions.init(allocator),
        };

        // Populate curr/peek
        p.next_token();
        p.next_token();

        return p;
    }

    pub fn deinit(self: *Self) void {
        self.errors.deinit();
        var i = self.exprs.items.len;
        while (i > 0) {
            i -= 1;
            self.allocator.destroy(self.exprs.items[i]);
        }
        self.exprs.deinit();
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
        std.log.warn("try to parse expression statement", .{});
        if (self.parse_expression(.lowest)) |e| {
            return .{ .exp = .{
                .value = e,
            } };
        }

        if (self.peek_token == .semicolon or self.peek_token == .eof) {
            self.next_token();
        } else {
            std.log.warn("Parsed expression statement but didn't reach the end: {s}", .{@tagName(self.peek_token)});
        }

        return null;
    }

    fn parse_expression(self: *Self, precedence: Precedence) ?Expression {
        if (self.parse_prefix()) |lhs| {
            var current_exp = lhs;

            while (self.peek_token != .semicolon and @intFromEnum(precedence) < @intFromEnum(token_precedence(self.peek_token))) {
                self.next_token();
                if (self.parse_infix(current_exp)) |e| {
                    current_exp = e;
                }
            }
            std.log.warn("tokens {s} {s}", .{ @tagName(self.curr_token), @tagName(self.peek_token) });
            std.log.warn("precedence {d} {d}", .{ @intFromEnum(precedence), @intFromEnum(token_precedence(self.peek_token)) });
            return current_exp;
        } else {
            return null;
        }
    }

    fn parse_prefix(self: *Self) ?Expression {
        return switch (self.curr_token) {
            .ident => |ident| .{ .identifier = ident },
            .int => |int| .{ .integer = .{
                .value = 0,
                .string = int,
            } },
            .bang => {
                self.next_token();
                var rhs = self.parse_expression(.prefix).?;
                return .{ .prefix = .{
                    .token = .bang,
                    .rhs = self.wrap_expr(rhs) catch return null,
                } };
            },
            .minus => {
                self.next_token();
                var rhs = self.parse_expression(.prefix).?;
                return .{ .prefix = .{
                    .token = .minus,
                    .rhs = self.wrap_expr(rhs) catch return null,
                } };
            },
            .l_paren => {
                self.next_token();

                var exp = self.parse_expression(.lowest);

                if (self.peek_token != .r_paren) {
                    std.log.warn("did not reach end of expression", .{});
                } else {
                    // This is to skip the right parenthesis, but the reference implementation doesn't use it.
                    self.next_token();
                }

                return exp;
            },
            else => null,
        };
    }

    fn wrap_expr(self: *Self, expr: Expression) !*Expression {
        const allocated = try self.allocator.create(Expression);
        allocated.* = expr;
        try self.exprs.append(allocated);
        return allocated;
    }

    fn parse_infix(self: *Self, lhs: Expression) ?Expression {
        std.log.warn("tried to parse infix: {s}", .{@tagName(self.curr_token)});
        switch (self.curr_token) {
            .plus,
            .minus,
            .multiply,
            .divide,
            .equal,
            .not_equal,
            .gt,
            .gte,
            .lt,
            .lte,
            => {
                var infix_exp: Expression = .{
                    .infix = .{
                        .op = self.curr_token,
                        .lhs = null,
                        .rhs = null,
                    },
                };

                var precedence = token_precedence(self.curr_token);
                self.next_token();
                var rhs = self.parse_expression(precedence).?;

                infix_exp.infix.lhs = self.wrap_expr(lhs) catch return null;
                infix_exp.infix.rhs = self.wrap_expr(rhs) catch return null;
                return infix_exp;
            },
            else => return (self.wrap_expr(lhs) catch return null).*,
        }
    }
};

// test "parse an infix expression" {
//     const src = "50 + 5;";
//     var parser = Parser.init(std.testing.allocator, src);
//     defer parser.deinit();
//     var program = try parser.parse_program();
//     defer program.deinit();
//     try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);
// }

// test "parse an infix operator with identifiers" {
//     const src = "3 + 4 * 5 == 3 * 1 + 4 * 5";
//     var parser = Parser.init(std.testing.allocator, src);
//     defer parser.deinit();
//     var program = try parser.parse_program();
//     defer program.deinit();
//     try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);
//     var list = ArrayList(u8).init(test_allocator);
//     var writer = list.writer();
//     defer list.deinit();
//     program.print(writer);
//     std.log.warn("{s}", .{list.items});
// }

test "parse a grouped expression" {
    const src = "c * (a + b)";
    var parser = Parser.init(std.testing.allocator, src);
    defer parser.deinit();
    var program = try parser.parse_program();
    defer program.deinit();
    var list = ArrayList(u8).init(test_allocator);

    var writer = list.writer();
    defer list.deinit();
    program.print(writer);
    std.log.warn("{d} ??? {s}", .{ program.statements.items.len, list.items });
}

// test "parse a let statement" {
//     var src =
//         \\let x = 5;
//     ;
//     var parser = Parser.init(std.testing.allocator, src);
//     defer parser.deinit();
//     var program = try parser.parse_program();
//     defer program.deinit();
//     try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);
//     try std.testing.expectEqualDeep(
//         Identifier{ .token = .{ .ident = "x" }, .value = "x" },
//         program.statements.items[0].let.identifier,
//     );
//     try std.testing.expectEqualStrings(
//         "5",
//         program.statements.items[0].let.value.integer.string,
//     );
//     try std.testing.expectEqual(parser.errors.items.len, 0);
// }

// test "parse no assign in let statement" {
//     var src =
//         \\let x 5;
//     ;
//     var parser = Parser.init(std.testing.allocator, src);
//     defer parser.deinit();
//     var program = try parser.parse_program();
//     defer program.deinit();
//     try std.testing.expectEqual(@as(usize, 1), parser.errors.items.len);
// }

// test "parse return statement" {
//     var src =
//         \\return 10;
//     ;
//     var parser = Parser.init(std.testing.allocator, src);
//     defer parser.deinit();
//     var program = try parser.parse_program();
//     defer program.deinit();
//     try std.testing.expectEqualStrings(
//         "10",
//         program.statements.items[0].ret.value.?.integer.string,
//     );
//     try std.testing.expectEqual(parser.errors.items.len, 0);
// }

// test "parse a singular integer expression statement" {
//     const src = "5;";
//     var parser = Parser.init(std.testing.allocator, src);
//     defer parser.deinit();
//     var program = try parser.parse_program();
//     defer program.deinit();
//     try std.testing.expectEqualStrings(
//         "5",
//         program.statements.items[0].exp.value.integer.string,
//     );
//     try std.testing.expectEqual(parser.errors.items.len, 0);
// }

// test "parse a singular identifier expression statement" {
//     const src = "foo;";
//     var parser = Parser.init(std.testing.allocator, src);
//     defer parser.deinit();
//     var program = try parser.parse_program();
//     defer program.deinit();
//     try std.testing.expectEqualStrings(
//         "foo",
//         program.statements.items[0].exp.value.identifier,
//     );
//     try std.testing.expectEqual(parser.errors.items.len, 0);
// }

// test "parse a prefix not operator" {
//     const src = "!foo;";
//     var parser = Parser.init(std.testing.allocator, src);
//     defer parser.deinit();
//     var program = try parser.parse_program();
//     defer program.deinit();
//     try std.testing.expectEqual(
//         program.statements.items[0].exp.value.prefix.token,
//         .bang,
//     );
//     try std.testing.expectEqual(parser.errors.items.len, 0);
// }

// test "parse a prefix negate operator" {
//     const src = "-55;";
//     var parser = Parser.init(std.testing.allocator, src);
//     defer parser.deinit();
//     var program = try parser.parse_program();
//     defer program.deinit();
//     try std.testing.expectEqual(
//         program.statements.items[0].exp.value.prefix.token,
//         .minus,
//     );
//     try std.testing.expectEqualStrings(
//         "55",
//         program.statements.items[0].exp.value.prefix.rhs.?.integer.string,
//     );
//     try std.testing.expectEqual(parser.errors.items.len, 0);
// }
