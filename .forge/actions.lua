local publish = [[set -euo pipefail; python3 .forge/publish]]

return {
    select = function(ctx)
        if string.match(ctx.ref_name, "^refs/tags/v%d+%.%d+%.%d+") then
            return { "publish" }
        end
        return {}
    end,
    publish = function(_)
        return {
            output = "package",
            base = publish,
            packages = {
                -- Read access makes the sandbox write a NuGet.Config that goes
                -- through its credentialed gateway. Without it the restore
                -- queries the registry anonymously and fails with 401.
                read = {
                    { registry = "41021897", ecosystem = "nuget" },
                },
                publish = {
                    { registry = "41021897", ecosystem = "nuget", mode = "explicit", prefix = "Rbt.Vb" },
                },
            },
        }
    end,
}
