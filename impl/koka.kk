div effect co<e>
  ctl yield () : unit
  ctl fork (p : () -> <co<e>,div|e> unit) : unit

effect queue<a>
  ctl enqueue (x : a) : unit
  ctl dequeue () : maybe<a>


div type proc<e>
  Proc (p :() -> <queue<proc<e>>,div|e> unit)

fun scheduler(p : () -> <co<e>, queue<proc<e>>, div|e> unit) : <queue<proc<e>>,div|e> unit
  with handler {
    return (_)
      match dequeue()
        Nothing -> ()
        Just (Proc (p')) -> p'()
    ctl yield ()
      enqueue (Proc ({ resume (()) }))
      match dequeue()
        Nothing -> ()
        Just (Proc (p')) -> p'()
    ctl fork(p')
      enqueue (Proc ({ scheduler({ mask<queue> (p') }) }))
      resume(())
  }
  p ()

fun main ()
    println("Hello world")
