Elm.Native.Transform2DApply = {};
Elm.Native.Transform2DApply.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Transform2DApply = localRuntime.Native.Transform2DApply || {};
    if (localRuntime.Native.Transform2DApply.values)
    {
        return localRuntime.Native.Transform2DApply.values;
    }

    var Utils = Elm.Native.Utils.make(localRuntime);

    function apply(m, x, y)
    {
        var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4], mdx = m[2], mdy = m[5];
        return Utils.Tuple2(m11 * x + m12 * y + mdx, m21 * x + m22 * y + mdy);
    }

    return localRuntime.Native.Transform2DApply.values = {
        apply:F3(apply)
    };
};
