// written by Codex

scoped effect queues<s::S>
  ctl scope() : unit

named effect queue<s::S,a::V> in queues<s>
  fun enqueue(x : a) : unit
  fun dequeue() : maybe<a>

alias proc-eff<s::S> = <queues<s>,console,exn,div>

named div effect co<s::S> in queues<s>
  ctl yield() : unit
  ctl fork(p : ev<co<s>> -> proc-eff<s> unit) : unit

div type proc<s::S>
  Proc(p : ev<queue<s,proc<s>>> -> proc-eff<s> unit)

fun run-next(q : ev<queue<s,proc<s>>>) : proc-eff<s> unit
  match q.dequeue()
    Nothing -> ()
    Just(Proc(p)) -> p(q)

fun schedule(q : ev<queue<s,proc<s>>>, p : ev<co<s>> -> proc-eff<s> unit)
    : proc-eff<s> unit
  with c <- named handler
    return(_)
      run-next(q)
    ctl yield()
      q.enqueue(Proc(fn(q') resume(())))
      run-next(q)
    ctl fork(p')
      q.enqueue(Proc(fn(q') schedule(q',p')))
      resume(())
  p(c)

fun with-queue(action : ev<queue<s,a>> -> e b) : e b
  var q := []
  with qh <- named handler
    fun enqueue(x)
      q := q ++ [x]
    fun dequeue()
      match q
        Nil -> Nothing
        Cons(x,xs) ->
          q := xs
          Just(x)
  action(qh)

fun scheduler(p : forall<s> ev<co<s>> -> proc-eff<s> unit)
    : <console,exn,div> unit
  with ctl scope() resume(())
  with-queue(fn(q) schedule(q,fn(c) p(c)))

fun example(c : ev<co<s>>) : proc-eff<s> unit
  println("main: start")
  c.fork(fn(cchild)
    println("child: before yield")
    cchild.yield()
    println("child: after yield")
  )
  println("main: after fork")
  c.yield()
  println("main: after yield")

fun main()
  scheduler(example)
