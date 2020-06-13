type pos = int * int

type 'a t =
  { obj : 'a
  ; start : pos
  ; stop : pos
  }

let create (obj : 'a) (start : pos) (stop : pos) = { obj; start; stop }
let obj mark = mark.obj
let map mark ~f = { obj = f mark.obj; start = mark.start; stop = mark.stop }
let start mark = mark.start
let stop mark = mark.stop
let with_mark obj mark = { obj; start = mark.start; stop = mark.stop }
let create_from_range head tail obj = { obj; start = head.start; stop = tail.stop }
