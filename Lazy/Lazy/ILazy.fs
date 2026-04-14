namespace Lazy

/// <summary>
/// Returns the cached value or computes it on the first call.
/// </summary>
type ILazy<'a> =
    abstract member Get : unit -> 'a