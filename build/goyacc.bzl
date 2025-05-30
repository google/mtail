"""Provides go_yacc rule."""

_GO_YACC_TOOL = "@org_golang_x_tools//cmd/goyacc"

def _go_yacc_impl(ctx):
    args = ctx.actions.args()
    args.add("-o", ctx.outputs.out)
    args.add("-p", ctx.attr.prefix)
    args.add(ctx.file.src)
    goroot = "%s/.." % ctx.executable._go_yacc_tool.dirname
    ctx.actions.run(
        mnemonic = "GoYacc",
        executable = ctx.executable._go_yacc_tool,
        arguments = [args],
        inputs = [ctx.file.src],
        outputs = [ctx.outputs.out],
        env = {
            "GOROOT": goroot,
        },
    )
    return DefaultInfo(
        files = depset([ctx.outputs.out]),
    )

go_yacc = rule(
    implementation = _go_yacc_impl,
    attrs = {
        "src": attr.label(
            allow_single_file = True,
        ),
        "out": attr.output(),
        "prefix": attr.string(),
        "_go_yacc_tool": attr.label(
            default = _GO_YACC_TOOL,
            allow_single_file = True,
            executable = True,
            cfg = "exec",
        ),
    },
)
