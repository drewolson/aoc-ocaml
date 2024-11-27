let ( let* ) t f = Core.List.bind t ~f
let ( and* ) = Core.List.Let_syntax.Let_syntax.both
let ( let+ ) t f = Core.List.map t ~f
let ( and+ ) = Core.List.Let_syntax.Let_syntax.both
let return = Core.List.return
