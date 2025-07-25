package caches.sim

import org.scalatest.funsuite.AnyFunSuite

class TraceTrafficTests2 extends AnyFunSuite {

  var rand: scala.util.Random = new scala.util.Random;

  test("Empty") {
    var traf = new TraceTraffic(1, Array.empty.iterator, (_) => ())

    assert(Range.apply(0,100).forall(_ => traf.requestMemoryAccess().isEmpty))
  }

  test("Single") {
    var traf = new TraceTraffic(1, Array(new MemAccess(0,true, 0, 0)).iterator, (_) => ())

    assert(traf.requestMemoryAccess().isDefined)
    traf.serveMemoryAccess(())
    assert(Range.apply(0,100).forall(_ => traf.requestMemoryAccess().isEmpty))
  }

  test("Single Double Size") {
    var traf = new TraceTraffic(1, Array(new MemAccess(1,true, 0, 0)).iterator, (_) => ())

    assert(traf.requestMemoryAccess().contains((0,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((1,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Single Double Size 2") {
    var traf = new TraceTraffic(2, Array(new MemAccess(2,true, 10, 0)).iterator, (_) => ())

    assert(traf.requestMemoryAccess().contains((10,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((12,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Single Quad Size") {
    var traf = new TraceTraffic(2, Array(new MemAccess(3,true, 20, 0)).iterator, (_) => ())

    assert(traf.requestMemoryAccess().contains((20,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((22,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((24,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((26,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Two accesses") {
    var traf = new TraceTraffic(4, Array(
      new MemAccess(0,true, 20, 0),
      new MemAccess(0,true, 56, 0)
    ).iterator,(_) => ())

    assert(traf.requestMemoryAccess().contains((20,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((56,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Double size before access") {
    var traf = new TraceTraffic(2, Array(
      new MemAccess(2,true, 20, 0),
      new MemAccess(0,true, 56, 0)
    ).iterator,(_) => ())

    assert(traf.requestMemoryAccess().contains((20,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((22,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((56,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Twice oversize") {
    var traf = new TraceTraffic(1, Array(
      new MemAccess(2,true, 16, 0),
      new MemAccess(1,true, 90, 0)
    ).iterator,(_) => ())

    assert(traf.requestMemoryAccess().contains((16,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((17,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((18,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((19,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((90,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((91,true,())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Delayed") {
    var delay = rand.nextInt(300)
    var traf = new TraceTraffic(1, Array(
      new MemAccess(0,true, 16, delay)
    ).iterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((16,true,())))
    traf.serveMemoryAccess(())
    traf.triggerCycle()
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Delayed 2") {
    var delay = rand.nextInt(300)
    var delay2 = delay + rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(1, Array(
      new MemAccess(0,true, 16, delay),
      new MemAccess(0,true, 74, delay2)
    ).iterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((16,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(Range.apply(0, delay2-delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((74,true,())))
  }

  test("Delayed with Double size") {
    var delay = rand.nextInt(300)
    var delay2 = delay + rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(1, Array(
      new MemAccess(1,true, 16, delay),
      new MemAccess(0,true, 74, delay2)
    ).iterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((16,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((17,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(Range.apply(0, delay2-delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((74,true,())))
  }

  test("Unaligned burst-2 data-2") {
    var delay = rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(2, Array(
      new MemAccess(1,true, 1, delay),
    ).iterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((0,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((2,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Unaligned burst-2 data-4") {
    var delay = rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(2, Array(
      new MemAccess(2,true, 1, delay),
    ).iterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((0,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((2,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((4,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Aligned burst-2 data-4") {
    var delay = rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(2, Array(
      new MemAccess(2,true, 6, delay),
    ).iterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((6,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((8,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Unaligned burst-2 data-8") {
    var delay = rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(2, Array(
      new MemAccess(3,true, 11, delay),
    ).iterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((10,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((12,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((14,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((16,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((18,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Burst-unaligned burst-8 data-4") {
    var delay = rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(8, Array(
      new MemAccess(2,true, 4, delay),
    ).iterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((0,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Burst-unaligned data-unaligned burst-8 data-4") {
    var delay = rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(8, Array(
      new MemAccess(2,true,3, delay),
    ).iterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((0,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Burst-boundary crossing access") {
    var delay = rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(8, Array(
      new MemAccess(2,true,5, delay),
    ).iterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((0,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((8,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Report done") {
    var delay = rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(1, Array(
      new MemAccess(0,true,1, delay),
      new MemAccess(0,true,2, delay),
      new MemAccess(0,true,3, delay),
    ).iterator,(_) => ())

    assert(!traf.isDone())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      assert(!traf.isDone())
      result
    }))
    assert(traf.requestMemoryAccess().contains((1,true,())))
    assert(!traf.isDone())
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      assert(!traf.isDone())
      result
    }))
    traf.serveMemoryAccess(())
    assert(!traf.isDone())
    assert(traf.requestMemoryAccess().contains((2,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      assert(!traf.isDone())
      result
    }))
    traf.serveMemoryAccess(())
    assert(!traf.isDone())
    assert(traf.requestMemoryAccess().contains((3,true,())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      assert(!traf.isDone())
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.isDone())
  }

}


class NoTraffic(burst: Int) extends Traffic[Unit] {
  override def burstSize: Int = burst

  override def serveMemoryAccess(token: Unit): Boolean = true

  override def requestMemoryAccess(): Option[(Long, Boolean, Unit)] = None;

  override def triggerCycle(): Unit = {}

  override def isDone(): Boolean = false;
}

class SingleTraffic(burst: Int, addr: Long, isRead:Boolean) extends Traffic[Unit] {
  override def burstSize: Int = burst

  override def serveMemoryAccess(token: Unit): Boolean = true

  override def requestMemoryAccess(): Option[(Long, Boolean, Unit)] = Some((addr, isRead, ()));

  override def triggerCycle(): Unit = {}

  override def isDone(): Boolean = false;
}

class RoundRobinArbiterTests extends AnyFunSuite {
  var rand: scala.util.Random = new scala.util.Random;
  test("No Accesses") {
    var arbiter = new RoundRobinArbiter(8, 1,
      Array.fill(2) {new NoTraffic(1)},(_) => None,
    )

    for (_ <- 0 until 10) {
      assert(arbiter.requestMemoryAccess().isEmpty)
      arbiter.triggerCycle()
    }
  }

  test("Serve after latency") {
    val latency = 1+rand.nextInt(25)
    var arbiter = new RoundRobinArbiter(8,latency,
      Array.range(0,3).map(idx => new ArrayTrafficUnit(Array((idx, true)), ()=>())),(_) => None,
    )

    assert(arbiter.requestMemoryAccess().contains((0,true,0))) // First core access
    // Insert arbitrary wait for serve
    for(_ <- 0 until rand.nextInt(30)) arbiter.triggerCycle();
    arbiter.serveMemoryAccess(0)
    // busy servicing access
    for(_ <- 0 until latency) {
      assert(arbiter.requestMemoryAccess().isEmpty)
      arbiter.triggerCycle()
    }

    var req2:Option[(Long, Boolean, Int)] = None
    while(req2.isEmpty) {
      req2 = arbiter.requestMemoryAccess()
      arbiter.triggerCycle()
    }
    var oneDone = req2.contains((1,true,1))
    var twoDone = req2.contains((2,true,2))
    assert(oneDone ^ twoDone) // ready to service next access

    // Insert arbitrary wait for serve
    for(_ <- 0 until rand.nextInt(30)) arbiter.triggerCycle();
    arbiter.serveMemoryAccess(req2.get._3)
    // busy servicing
    for(_ <- 0 until latency) {
      assert(arbiter.requestMemoryAccess().isEmpty)
      arbiter.triggerCycle()
    }

    var req3:Option[(Long, Boolean, Int)] = None
    while(req3.isEmpty) {
      req3 = arbiter.requestMemoryAccess()
      arbiter.triggerCycle()
    }
    oneDone = oneDone || req3.contains((1,true,1))
    twoDone = twoDone || req3.contains((2,true,2))
    assert(twoDone && oneDone) // ready to service next access
  }

  test("Triggers device cycles") {
    var counts = Array.fill(4){0}
    var arbiter = new RoundRobinArbiter(8,
      6,
      Array.range(0,4).map(idx => new Traffic[Unit] {
        def burstSize: Int = 1;

        def triggerCycle() = {counts(idx) += 1};

        def requestMemoryAccess(): Option[(Long, Boolean, Unit)] = {
          if(math.random() < 0.5) Some((0, true, ())) else None
        };

        def serveMemoryAccess(token: Unit):Boolean = true;

        override def isDone(): Boolean = false;
      }), (_) => None,
    )

    val triggerCount = rand.nextInt(256)
    Range.apply(0, triggerCount).foreach(_=> arbiter.triggerCycle())
    counts.foreach(c => assert(c == triggerCount))
  }

  test("Report done") {
    var latency = 2+rand.nextInt(25)
    var done = false;
    var arbiter = new RoundRobinArbiter(8,
      latency,
      Array(new TraceTraffic(1, Array(
        new MemAccess(0,true, 20, 0)
      ).iterator,(_) => ())),
      (_) => {
        done = !done
        None
      },
    )

    assert(!arbiter.isDone())
    assert(!done)
    assert(arbiter.requestMemoryAccess().contains((20,true,0)))
    assert(!arbiter.isDone())
    assert(!done)
    arbiter.triggerCycle()
    assert(!arbiter.isDone())
    assert(!done)
    arbiter.serveMemoryAccess(0)
    for(i <- 0 until latency+1) {
      assert(!arbiter.isDone())
      assert(!done)
      arbiter.triggerCycle()
    }
    assert(done)
  }

  test("Report delayed done") {
    var latency = 1+rand.nextInt(25)
    var done = false;
    val doneLatency = 1+rand.nextInt(10)
    var arbiter = new RoundRobinArbiter(8,
      latency,
      Array(new Traffic[Unit] {
        var countdown:Option[Int] = None
        var requested = false
        override def burstSize: Int = 1

        override def serveMemoryAccess(token: Unit): Boolean = {
          assert(countdown.isEmpty)
          countdown = Some(doneLatency)
          true
        }

        override def  requestMemoryAccess(): Option[(Long, Boolean, Unit)] = {
          if(!requested) {
            requested = true
            Some((0, true, ()))
          } else {
            None
          }
        }

        override def triggerCycle(): Unit = {
          if(countdown.isDefined && countdown.get >0){
            countdown = Some(countdown.get - 1)
          }
        }

        override def isDone():Boolean = {
          countdown.isDefined && (countdown.get == 0) && requested
        }
      }),
      (_) => {
        done = !done
        None
      },
    )

    assert(arbiter.requestMemoryAccess().contains((0,true,0)))
    assert(!arbiter.isDone())
    assert(!done)
    arbiter.triggerCycle()
    assert(!arbiter.isDone())
    assert(!done)
    arbiter.serveMemoryAccess(0)
    for(i <- 0 until 1+latency+doneLatency) {
      assert(!arbiter.isDone())
      assert(!done)
      arbiter.triggerCycle()
    }
    assert(arbiter.isDone())
    assert(done)
  }

  test("Change On Done") {
    var latency = 2+rand.nextInt(25)
    val newTraffic = () => new TraceTraffic(1, Array(
      new MemAccess(0, true, 20, 0)
    ).iterator, (_) => ())


    var arbiter = new RoundRobinArbiter(8,
      latency,
      Array(newTraffic()),
      (_) => {
        Some(newTraffic())
      },
    )

    assert(arbiter.requestMemoryAccess().contains((20,true,0)))
    arbiter.triggerCycle()
    arbiter.serveMemoryAccess(0)
    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone())
      arbiter.triggerCycle()
    }
    assert(!arbiter.isDone())

    assert(arbiter.requestMemoryAccess().contains((20,true,0)))
    arbiter.triggerCycle()
    arbiter.serveMemoryAccess(0)
    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone())
      arbiter.triggerCycle()
    }
    assert(!arbiter.isDone())
  }

  test("Allow multiple outstanding requests") {
    var latency = 2+rand.nextInt(25)
    var done = 0
    val newTraffic = (addr: Int) => new TraceTraffic(1, Array(
      new MemAccess(0, true, addr, 0)
    ).iterator, (_) => done += 1)

    var arbiter = new RoundRobinArbiter(8,
      latency,
      Array(newTraffic(20), newTraffic(40)),
      (_) => None,
      true // Allow multiple requests
    )

    assert(arbiter.requestMemoryAccess().contains((20,true,0)))
    arbiter.triggerCycle()
    assert(arbiter.requestMemoryAccess().contains((40,true,1))) // Second request issued while waiting for the first

    // wait some random amount of time before external response
    for(i <- 0 until 1+rand.nextInt(25)) {
      assert(!arbiter.isDone() && done == 0)
      arbiter.triggerCycle()
    }
    assert(arbiter.serveMemoryAccess(0)) // Respond to first

    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone() && done == 0)
      arbiter.triggerCycle()
    }
    assert(!arbiter.isDone() && done == 1) // First request done

    // wait some random amount of time before second external response
    for(i <- 0 until 1+rand.nextInt(25)) {
      assert(!arbiter.isDone() && done == 1)
      arbiter.triggerCycle()
    }
    assert(arbiter.serveMemoryAccess(1)) // Respond to second

    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone() && done == 1)
      arbiter.triggerCycle()
    }
    assert(arbiter.isDone() && done == 2) // Second request done
  }

  test("Multi-request external serve while internal serve") {
    var latency = 2+rand.nextInt(25)
    var done = 0
    val newTraffic = (addr: Int) => new TraceTraffic(1, Array(
      new MemAccess(0, true, addr, 0)
    ).iterator, (_) => done += 1)

    var arbiter = new RoundRobinArbiter(8,
      latency,
      Array(newTraffic(20), newTraffic(40)),
      (_) => None,
      true // Allow multiple requests
    )

    assert(arbiter.requestMemoryAccess().contains((20,true,0)))
    arbiter.triggerCycle()
    assert(arbiter.requestMemoryAccess().contains((40,true,1))) // Second request issued while waiting for the first

    // wait some random amount of time before external response
    for(i <- 0 until 1+rand.nextInt(25)) {
      assert(!arbiter.isDone() && done == 0)
      arbiter.triggerCycle()
    }
    assert(arbiter.serveMemoryAccess(0)) // Respond to first
    for(i <- 0 until 1+latency) {
      assert(!arbiter.serveMemoryAccess(1)) // Disallow serve of second while servicing first
      assert(!arbiter.isDone() && done == 0)
      arbiter.triggerCycle()
    }
    assert(!arbiter.isDone() && done == 1) // First request done
    assert(arbiter.serveMemoryAccess(1)) // Respond to second

    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone() && done == 1)
      arbiter.triggerCycle()
    }
    assert(arbiter.isDone() && done == 2) // Second request done
  }

  test("Multi-request Reply out of order") {
    var latency = 2+rand.nextInt(25)
    var done = Array.fill(3){false}
    val newTraffic = (coreId: Int, addr: Int) => new TraceTraffic(1, Array(
      new MemAccess(0, true, addr, 0)
    ).iterator, (_) => done(coreId) = true)
    val doneArray = (d1:Boolean, d2:Boolean, d3:Boolean) => {
      done(0) == d1 && done(1) == d2 && done(2) == d3
    }


    var arbiter = new RoundRobinArbiter(8,
      latency,
      Array(newTraffic(0,20), newTraffic(1,40), newTraffic(2,60)),
      (_) => None,
      true // Allow multiple requests
    )

    assert(arbiter.requestMemoryAccess().contains((20,true,0)))
    arbiter.triggerCycle()
    assert(arbiter.requestMemoryAccess().contains((40,true,1)))
    arbiter.triggerCycle()
    assert(arbiter.requestMemoryAccess().contains((60,true,2)))

    // wait some random amount of time before external response
    for(i <- 0 until 1+rand.nextInt(25)) {
      assert(!arbiter.isDone() && doneArray(false,false,false))
      arbiter.triggerCycle()
    }
    assert(arbiter.serveMemoryAccess(2)) // Respond to last first

    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone() && doneArray(false,false,false))
      arbiter.triggerCycle()
    }
    assert(!arbiter.isDone() && doneArray(false,false,true)) // last request done

    // wait some random amount of time before second external response
    for(i <- 0 until 1+rand.nextInt(25)) {
      assert(!arbiter.isDone() && doneArray(false,false,true))
      arbiter.triggerCycle()
    }
    assert(arbiter.serveMemoryAccess(1)) // Respond to second

    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone() && doneArray(false,false,true))
      arbiter.triggerCycle()
    }
    assert(!arbiter.isDone() && doneArray(false,true,true)) // Second request done

    // wait some random amount of time before third external response
    for(i <- 0 until 1+rand.nextInt(25)) {
      assert(!arbiter.isDone() && doneArray(false,true,true))
      arbiter.triggerCycle()
    }
    assert(arbiter.serveMemoryAccess(0)) // Respond to first

    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone() && doneArray(false,true,true))
      arbiter.triggerCycle()
    }
    assert(arbiter.isDone() && doneArray(true,true,true)) // First request done
  }

  test("Skipping core with outstanding request") {
    var latency = 2+rand.nextInt(25)
    var done = Array.fill(3){0}
    val newTraffic = (coreId: Int, addr: Int) => new TraceTraffic(1, Array(
      new MemAccess(0, true, addr, 0)
    ).iterator, (_) => done(coreId) += 1)
    val doneArray = (d1:Int, d2:Int, d3:Int) => {
      done(0) == d1 && done(1) == d2 && done(2) == d3
    }

    var arbiter = new RoundRobinArbiter(8,
      latency,
      Array(newTraffic(0,20), newTraffic(1,40), newTraffic(2,60)),
      (coreNr) => {
        Some(newTraffic(coreNr, 20 + (coreNr*20)))
      },
      true // Allow multiple requests
    )

    assert(arbiter.requestMemoryAccess().contains((20,true,0)))
    arbiter.triggerCycle()
    assert(arbiter.requestMemoryAccess().contains((40,true,1)))
    arbiter.triggerCycle()
    assert(arbiter.requestMemoryAccess().contains((60,true,2)))

    // wait some random amount of time before external response
    for(i <- 0 until 1+rand.nextInt(25)) {
      assert(!arbiter.isDone() && doneArray(0,0,0))
      arbiter.triggerCycle()
    }
    assert(arbiter.serveMemoryAccess(2)) // Respond to last first

    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone() && doneArray(0,0,0))
      arbiter.triggerCycle()
    }
    assert(!arbiter.isDone() && doneArray(0,0,1)) // last request done

    // At most after 3 cycles should
    var foundNextCore3Access = false
    for(i <- Range(0,3)) {
      val req = arbiter.requestMemoryAccess()
      assert((req.isEmpty || req.contains((60, true, 2))))
      foundNextCore3Access =  foundNextCore3Access || req.contains((60, true, 2))
      arbiter.triggerCycle()
    }

    assert(foundNextCore3Access)

    // wait some random amount of time before second external response
//    for(i <- 0 until 1+rand.nextInt(25)) {
//      assert(!arbiter.isDone() && doneArray(false,false,true))
//      arbiter.triggerCycle()
//    }
//    assert(arbiter.serveMemoryAccess(1)) // Respond to second
//
//    for(i <- 0 until 1+latency) {
//      assert(!arbiter.isDone() && doneArray(false,false,true))
//      arbiter.triggerCycle()
//    }
//    assert(!arbiter.isDone() && doneArray(false,true,true)) // Second request done
//
//    // wait some random amount of time before third external response
//    for(i <- 0 until 1+rand.nextInt(25)) {
//      assert(!arbiter.isDone() && doneArray(false,true,true))
//      arbiter.triggerCycle()
//    }
//    assert(arbiter.serveMemoryAccess(0)) // Respond to first
//
//    for(i <- 0 until 1+latency) {
//      assert(!arbiter.isDone() && doneArray(false,true,true))
//      arbiter.triggerCycle()
//    }
//    assert(arbiter.isDone() && doneArray(true,true,true)) // First request done
  }
  
}