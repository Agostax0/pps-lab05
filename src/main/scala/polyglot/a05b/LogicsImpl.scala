package polyglot.a05b

import polyglot.a05b.Logics
import util.Sequences.*
import util.Sequences.Sequence.{Cons, empty}

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */

/**
 *
 * * Scopo di questo esercizio è realizzare una GUI con l'aspetto mostrato nell'immagine fig1.png, fornita,
 * * che realizza una semplice animazione, che effettua uno scatto ad ogni pressione del pulsante ">", mostrata nella
 * * sequenza delle immagini fig1.png, fig2.png, fig3.png, fig4.png,..
 * * 1 - all'inizio la griglia è vuota, con una cella attiva (con asterisco) scelta in modo random *non nel bordo*
 * * 2 - ad ogni pressione si attivano 8 celle in più, in direzione orizzontale/verticale/diagonale rispetto a
 * *     quella iniziale, a distanza via via crescente
 * * 3 - quando l'aggiunta di una cella porterebbe a uscire dalla griglia, si esca
 * */

class LogicsImpl(private val size: Int) extends Logics:
  opaque type Position = (Int, Int)
  val initialPosition: Position = _init()
  var offset: Int = 1
  var positions: Sequence[Position] = Cons(initialPosition, empty)

  private def _init(): Position =
    val random = scala.util.Random
    (
      1 + random.nextInt(size - 2),
      1 + random.nextInt(size - 2)
    )

  override def tick(): Unit = {
    for
      offX <- -1 to 1
      offY <- -1 to 1
      if offX != 0 || offY != 0
    do
      println((offX, offY))
      val newPos = ( initialPosition._1 + offset * offX , initialPosition._2 + offset * offY )
      println("pos:" + newPos)
      positions = Cons(newPos, positions)
    offset += 1
  }

  override def isOver: Boolean = positions.find( (x,y) => x > size || y > size || x < 0 || y < 0).isEmpty

  override def hasElement(x: Int, y: Int): Boolean = positions.contains((x,y))
