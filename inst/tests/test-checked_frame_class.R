context("checked_frame_class")

Foo <- checked_frame_class("Foo",
                           columns =
                           ColumnCheckList(foo = ColumnChecks("numeric", constraints = FunctionList(function(x) x > 0))))

