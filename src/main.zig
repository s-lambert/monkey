const std = @import("std");

pub fn main() !void {}

const Token = union(enum) {
    eof,

    ident: []const u8,
    int: []const u8,

    assign,
    plus,

    comma,
    semicolon,

    l_paren,
    r_paren,
    l_brace,
    r_brace,

    function,
    let,

    illegal,

    pub fn keyword(ident: []const u8) ?Token {
        const map = std.ComptimeStringMap(Token, .{
            .{ "let", .let },
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
            '=' => .assign,
            ';' => .semicolon,
            'a'...'z', 'A'...'Z' => {
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
    const first_program =
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
    var lex = Lexer.init(first_program);

    for (expected_tokens) |token| {
        const tok = lex.next_token();

        try std.testing.expectEqualDeep(token, tok);
    }
}
