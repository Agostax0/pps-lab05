package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import polyglot.a01b.Logics
import util.Optionals.Optional.Just
import util.Sequences.Sequence
import util.Sequences.Sequence.Cons

import java.util.Optional
import scala.jdk.javaapi.OptionConverters

/*
     * Scopo di questo esercizio è realizzare una GUI con l'aspetto mostrato nell'immagine fig.png, fornita,
     * che realizza una mini-versione di Campo Minato. All'avvio si prepara un campo di gara quadrato
     * (dimensione indicata dal primo parametro del costruttore), e si collocano in modo random le mine (in numero
     * indicato nel secondo parametro) facendo attenzioni che siano in posizioni distinte
     * (si stampi su console la loro posizione, a fini di debug).
     * Alla pressione di un pulsante, questo si disabilita: se contiene una mina si esca indicando la sconfitta,
     * altrimenti si mostri sul pulsante quante mine sono presenti in un vicino della cella clickata
     * (in orizzontale, verticale o diagonale).
     * Colpite tutte le celle che non hanno delle mine, si avrà vinto: si produca una stampa a console e si esca.
     *
     * Sono considerati opzionali ai fini della possibilità di correggere l'esercizio, ma concorrono comunque al raggiungimento
     * della totalità del punteggio:
     * - scorporamento di tutti gli aspetti che non sono di view in una interfaccia+classe esterna, via Strategy
     * - compilazione e esecuzione dell'esercizio da linea di comando
     *
     * La classe GUIExample fornita include codice che potrebbe essere utile per la soluzione.
     *
     * Indicazioni di punteggio:
   * - correttezza della parte obbligatoria: 10 punti
   * - qualità della parte opzionale: 5 punti
   * - compilazione/esecuzione da linea di comando: 2 punti
     */
/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
trait LogicsTrait extends Logics:
  type Position

  def hit(x: Int, y: Int): Optional[Integer]
  def won: Boolean

class LogicsImpl(private val size: Int, private val numberOfMines: Int) extends LogicsTrait:
  override opaque type Position = (Int, Int)
  val mines: Sequence[Position] = _initMines()
  var hits: Int = 0

  def _initMines(): Sequence[Position] =
    var seq: Sequence[Position] = Sequence.empty
    var addedMines = 0
    val random = scala.util.Random
    while(addedMines < numberOfMines) {
      val minePos: Position = (random.nextInt(size), random.nextInt(size))
      if (!seq.contains(minePos))
        addedMines += 1
        seq = Cons(minePos, seq)
    }
    println(seq)
    seq

  object isMine:
    def unapply(pos: Position): Boolean = mines.contains(pos)

  def hit(x: Int, y: Int): java.util.Optional[Integer] = (x,y) match
    case isMine() => Optional.empty()
    case _ =>
      var acc: Int = 0;
      hits+=1;
      for
        offX <- -1 to 1
        offY <- -1 to 1
        if x != 0 || y != 0
      do
        if(mines.contains((x + offX, y + offY)))
          acc+= 1

      Optional.of(acc)

  def won = hits >= (size * size - numberOfMines)
