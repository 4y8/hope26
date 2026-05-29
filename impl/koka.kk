div effect co<e>
  ctl yield () : unit
  ctl fork (p : () -> <co<e>,div|e> unit) : unit

effect queue<a>
  ctl enqueue (x : a) : unit
  ctl dequeue () : maybe<a>

div type proc<e>
  Proc (p :() -> <queue<proc<e>>,div|e> unit)

fun run_next() : <queue<proc<e>>, div|e> unit
  match dequeue()
    Nothing -> ()
    Just (Proc (p)) -> p()

fun schedule(p : () -> <co<e>, queue<proc<e>>, div|e> unit) 
    : <queue<proc<e>>,div|e> unit
  with handler
    return (_)
      run_next()
    ctl yield ()
      enqueue (Proc ({ resume (()) }))
      run_next()
    ctl fork(p')
      enqueue (Proc ({ schedule({ mask<queue> (p') }) }))
      resume(())
  p ()

fun main ()
    println("Hello world")
