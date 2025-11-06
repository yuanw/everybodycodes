const std = @import("std");

pub fn main() !void {
    const fileName = "../../data/2022/day1.txt";
    const file = try std.fs.cwd().openFile(fileName, .{});
    defer file.close();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const read_buf = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(read_buf);

    var it = std.mem.split(u8, read_buf, "\n");

    var sum: u32 = 0;
    var max1: u32 = 0;
    var max2: u32 = 0;
    var max3: u32 = 0;

    while (it.next()) |amount| {
        if (amount.len > 0) {
            const result = try std.fmt.parseInt(u32, amount, 10);
            // std.debug.print("{}\n", .{result});
            sum += result;
        } else {
            if (sum > max1) {
                max3 = max2;
                max2 = max1;
                max1 = sum;
            } else if (sum > max2) {
                max3 = max2;
                max2 = sum;
            } else if (sum > max3) {
                max3 = sum;
            }
            sum = 0;
        }
    }
    std.debug.print("part I: max is {}\n", .{max1});
    std.debug.print("part II: max is {}\n", .{max1 + max2 + max3});
}
