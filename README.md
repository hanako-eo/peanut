# Peanut

Peanut, a small and powerful experimental scripting language, is in WIP.
It is an attempt to make a garbage collector as small and fast as possible.

## syntax (not final)

```pn
// execution flags
// @[right-types-influx] change (arg1, arg2 string) == (arg1 auto, arg2 string) to (arg1 string, arg2 string)

// arg1 is in auto type
func test(arg1, arg2 string) bool {
    print("{} is the type of arg1", type$(arg1))
    print("{} is the type of arg2", type$(arg2))
    return arg1 == arg2
}
func short_join_fn(...args string[]) (r string) => for arg in args r += arg

let is_eq = test("Hello", "Hello")

if is_eq print("Yes")
else print("No")
```