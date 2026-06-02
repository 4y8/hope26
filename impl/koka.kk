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
      enqueue (Proc ({ schedule({ mask<queue> (p') })}))
      resume(())
      
  p ()


// BEGIN: code for running examples
fun run_queue<a>(action : () -> <queue<a>,div|e> b) : <div|e> b
  var q := []
  handle action
    return(x) x
    fun enqueue(x)
      q := q ++ [x]
    fun dequeue()
      match q
        Nil -> Nothing
        Cons(x,xs) ->
          q := xs
          Just(x)

fun scheduler(p : () -> <co<e>,queue<proc<e>>,div|e> unit) : <div|e> unit
  run_queue { schedule(p) }

fun example() : <co<<console>>,queue<proc<<console>>>,console,div> unit
  println("main: start")
  fork(fn()
    println("child: before yield")
    yield()
    println("child: after yield")
  )
  println("main: after fork")
  yield()
  println("main: after yield")
// END: code for running examples

fun main ()
  scheduler(example)
