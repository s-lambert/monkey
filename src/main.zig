const repl = @import("./repl.zig");

pub fn main() !void {
    return repl.run_repl();
}
