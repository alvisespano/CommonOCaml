
let logger = new LogLib.color_console_logger ()

let pool = new PoolLib.thread_pool ~name:"pool" ~max:10 ~inactivity_timeout:1.0 ~inactivity_check_interval:1.0 logger

let f secs = logger#msg Log.Normal "ciao"; Thread.delay secs; logger#msg Log.Normal "bye"

;;

while true do
    let a = Random.float 0.8
    and b = Random.float 1.0
    in
        ignore (pool#spawn "caz" f b);
        Thread.delay a
done
