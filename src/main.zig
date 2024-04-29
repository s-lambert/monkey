const std = @import("std");

pub fn main() !void {}

const Token = union(enum) {
    illegal,
    eof,

    // Literals
    ident: []const u8,
    int: []const u8,

    // Operators
    assign,
    bang,
    not_equal,
    equal,
    plus,

    // Delimiters
    comma,
    semicolon,
    l_paren,
    r_paren,
    l_brace,
    r_brace,

    // Keywords
    function,
    let,

    pub fn keyword(ident: []const u8) ?Token {
        const map = std.ComptimeStringMap(Token, .{
            .{ "let", .let },
            .{ "fn", .function },
        });
        return map.get(ident);
    }
};

const Lexer = struct {
    const Self = @This();

    read_position: usize = 0,
    position: usize = 0,
    ch: u8 = 0,
    input: []const u8,

    pub fn init(input: []const u8) Self {
        var self = Self{
            .input = input,
        };

        self.move_next();

        return self;
    }

    pub fn next_token(self: *Self) Token {
        self.skip_whitespace();
        const token: Token = switch (self.ch) {
            0 => .eof,
            '+' => .plus,
            '!' => blk: {
                if (self.peek_char() == '=') {
                    self.move_next();
                    break :blk .not_equal;
                } else {
                    break :blk .bang;
                }
            },
            '=' => blk: {
                if (self.peek_char() == '=') {
                    self.move_next();
                    break :blk .equal;
                } else {
                    break :blk .assign;
                }
            },
            ',' => .comma,
            ';' => .semicolon,
            '(' => .l_paren,
            ')' => .r_paren,
            '{' => .l_brace,
            '}' => .r_brace,
            'a'...'z', 'A'...'Z', '_' => {
                const ident = self.read_identifier();
                if (Token.keyword(ident)) |token| {
                    return token;
                }
                return .{ .ident = ident };
            },
            '0'...'9' => {
                const int = self.read_int();
                return .{ .int = int };
            },
            else => .illegal,
        };

        self.move_next();
        return token;
    }

    fn skip_whitespace(self: *Self) void {
        while (std.ascii.isWhitespace(self.ch)) {
            self.move_next();
        }
    }

    fn peek_char(self: *Self) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        } else {
            return self.input[self.read_position];
        }
    }

    fn move_next(self: *Self) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(self: *Self) []const u8 {
        const position = self.position;

        while (std.ascii.isAlphabetic(self.ch)) {
            self.move_next();
        }

        return self.input[position..self.position];
    }

    fn read_int(self: *Self) []const u8 {
        const position = self.position;

        while (std.ascii.isDigit(self.ch)) {
            self.move_next();
        }

        return self.input[position..self.position];
    }
};

test "parse variable assignment" {
    const variable_assignment =
        \\let x = 5 + 5;
    ;
    const expected_tokens = [_]Token{
        .let,
        .{ .ident = "x" },
        .assign,
        .{ .int = "5" },
        .plus,
        .{ .int = "5" },
        .semicolon,
    };
    var lex = Lexer.init(variable_assignment);

    for (expected_tokens) |token| {
        const tok = lex.next_token();

        try std.testing.expectEqualDeep(token, tok);
    }
}

test "parse functions and calls" {
    const function_calls =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\  x + y;
        \\};
        \\let result = add(five, ten);
    ;

    const expected_tokens = [_]Token{
        .let,
        .{ .ident = "five" },
        .assign,
        .{ .int = "5" },
        .semicolon,
        .let,
        .{ .ident = "ten" },
        .assign,
        .{ .int = "10" },
        .semicolon,
        .let,
        .{ .ident = "add" },
        .assign,
        .function,
        .l_paren,
        .{ .ident = "x" },
        .comma,
        .{ .ident = "y" },
        .r_paren,
        .l_brace,
        .{ .ident = "x" },
        .plus,
        .{ .ident = "y" },
        .semicolon,
        .r_brace,
        .semicolon,
        .let,
        .{ .ident = "result" },
        .assign,
        .{ .ident = "add" },
        .l_paren,
        .{ .ident = "five" },
        .comma,
        .{ .ident = "ten" },
        .r_paren,
        .semicolon,
    };

    var lex = Lexer.init(function_calls);

    for (expected_tokens) |token| {
        const tok = lex.next_token();

        try std.testing.expectEqualDeep(token, tok);
    }
}

test "parse two-letter comparators" {
    const comparators =
        \\5 != 10;
        \\5 == 5;
    ;

    const expected_tokens = [_]Token{
        .{ .int = "5" },
        .not_equal,
        .{ .int = "10" },
        .semicolon,
        .{ .int = "5" },
        .equal,
        .{ .int = "5" },
        .semicolon,
    };

    var lex = Lexer.init(comparators);

    for (expected_tokens) |token| {
        const tok = lex.next_token();

        try std.testing.expectEqualDeep(token, tok);
    }
}
