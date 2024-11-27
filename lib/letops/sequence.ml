let ( let* ) t f = Core.Sequence.bind t ~f
let ( and* ) = Core.Sequence.Let_syntax.Let_syntax.both
let ( let+ ) t f = Core.Sequence.map t ~f
let ( and+ ) = Core.Sequence.Let_syntax.Let_syntax.both
let return = Core.Sequence.Let_syntax.Let_syntax.return
