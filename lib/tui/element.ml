type element = ..
type text = { lines : string list; style : Style.style option }
type element += Text of text
type row = { items : element list; style : Style.style option }
type element += Row of row
type column = { items : element list; style : Style.style option }
type element += Column of column
type table_row = { items : element list; style : Style.style option }
type table = { rows : table_row list; style : Style.style option }
type element += Table of table

let text ?style lines = Text { lines; style }
let row ?style items = Row { items; style }
let column ?style items = Column { items; style }

let table_row ?style items =
  let ret : table_row = { items; style } in
  ret

let table ?style rows = Table { rows; style }
