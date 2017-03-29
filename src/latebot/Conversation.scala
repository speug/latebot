package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue
import scala.util.Try

abstract class Conversation(val recipient: String, val incoming: Queue[(Long, String)], val out: BufferedWriter, val homeChannel: String, val bot: latebot, val historySize: Int, val messageHistory: Queue[(Long, String)]) extends Runnable {

  private val random = new Random
  private var stop = false
  private var savingQuote = false
  private var quoteToConfirm = ""

  val helpMessage =
    """LATEBOT v1.0(Release party?) -Quotable-
Lähdekoodi: https://bitbucket.org/Speug/latebot
 
Tämänhetkiset ominaisuudet
!answer:          Antaa kvanttikenttäfluktuaattorista oikean vastauksen kyllä/ei kysymykseen
!dice <x>d<y>     Heittää x kappaletta y-tahkoista noppaa.
!planned          Tulostaa suunnittellut ominaisuudet.
!changelog        Tulostaa viimeaikaiset muutokset.
!bigredbutton     Elä kajoa.
!terminate        Terminoi.
!stats            Kertoo kivasti tietoja. Käytä miel. queryssä.
!quote            Lukee lainauksen aikamme sankareilta
!quote "<quote>" -<author>: lisää lainauksen tietokantaan
!quote N#         lisää # viestiä sitten olleen viestin tietokantaan. Viimeisin viesti komennolla N1.
!irchelp          lähettää irc-apuviestin
 
Metodit testauksen alla, saa kokeilla. Ilmoita bugeista querylla nickille speug."""
  
  /**
   * Returns the command word in a given string.
   *
   * @param line a string possibly containing a keyword, marked with a '!'
   * @tparam line String
   *
   * @returns a command word, if such exists; otherwise an empty string.
   */

  def findCommand(line: String) = {
    line.split(":").last.dropWhile(_ != '!').takeWhile(_ != ' ').trim()
  }

  def run(): Unit = {
    while (!this.stop && !this.incoming.isEmpty) {
      if (!this.incoming.isEmpty && !this.savingQuote) {
        val line = this.incoming.dequeue()
        val lineString = line._2
        var nick = ""
        var receivedFrom = ""
        val dataSplit = lineString.split(":")
        if (lineString.contains("PRIVMSG")) {
          nick = dataSplit(1).split("!")(0)
          receivedFrom = this.address(lineString)
        }
        val command = findCommand(lineString)
        if (lineString.contains("PRIVMSG")) {
          nick = dataSplit(1).split("!")(0)
          receivedFrom = this.address(lineString)
          this.takeLine(line, nick)
        }
        command match {
          case "!answer"       => this.eightBall(out, receivedFrom)
          case "!dice"         => this.dice(lineString, out, receivedFrom)
          case "!help"         => this.scroller(out, nick, helpMessage)
          case "!terminate"    => this.terminate(out, nick)
          case "!bigredButton" => this.bigRedButton(out, nick)
          case "!relay"        => this.relay(out, lineString)
          case "!opme"         => this.opme(out, nick)
          case "!planned"      => this.plannedFeatures(out, lineString, receivedFrom, nick)
          case "!changelog"    => this.fileReader(out, receivedFrom, "changeLog.txt")
          case "!stats"        => this.stats(out)
          case "!quote"        => this.quote(out, lineString)
          case _               =>
        }
      } else if (this.savingQuote) {
        this.confirmQuote(this.quoteToConfirm)
        this.quoteToConfirm = ""
        this.savingQuote = false
      } else {
        this.synchronized {
          // println(this.recipient + " going into wait mode.")
          this.wait()
          // println(this.recipient + " has been notified.")
        }
      }
    }
  }

  def sendData(out: BufferedWriter, ircDataOutput: String) = {
    out.write(ircDataOutput)
    if (!ircDataOutput.contains("pong")) {
      println(ircDataOutput)
    }
    out.newLine()
    out.flush()
  }

  def sendMessage(out: BufferedWriter, message: String, receiver: String) = {
    val toBeSent = "PRIVMSG " + receiver + " :" + message
    sendData(out, toBeSent)
    this.messageHistory += ((System.currentTimeMillis(), toBeSent))
  }

  def address(line: String): String = {
    var recipient = line.split("PRIVMSG ")(1).takeWhile(_ != ' ')
    if (recipient(0) == '#') {
      recipient
    } else {
      recipient = line.split(":")(1).split("!")(0)
      recipient
    }
  }

  def dice(line: String, out: BufferedWriter, receiver: String) {
    if (line.split("!dice ").size >= 2) {
      val parameters = line.split("!dice ")(1).split('d')
      var amount = parameters.lift(0).getOrElse("0").trim()
      var faces = parameters.lift(1).getOrElse("0").trim()
      if (amount.forall(_.isDigit) && amount.length < 3 && !amount.isEmpty()) { amount = amount } else { amount = "0" }
      if (faces.forall(_.isDigit) && faces.length < 5 && !faces.isEmpty()) { faces = faces } else { faces = "0" }
      if (amount == "0" || faces == "0") {
        sendMessage(out, "Tarkasta syntaksi, !dice (noppien lukumäärä)d(tahkojen lukumäärä)", this.homeChannel)
      } else {
        var throwArray = Array.ofDim[Int](amount.toInt)
        for (i <- throwArray.indices) {
          throwArray(i) = random.nextInt(faces.toInt) + 1
        }
        val total = throwArray.sum
        val message = "Heitit " + throwArray.mkString(" + ") + " = " + total + "!"
        sendMessage(out, message, receiver)
      }
    } else {
      sendMessage(out, "Unohtuivatko parametrit?", receiver)
    }
  }

  def scroller(out: BufferedWriter, address: String, textToScroll: String) = {
    textToScroll.split("\n").foreach(sendMessage(out, _, address))
  }

  def eightBall(out: BufferedWriter, receivedFrom: String) = {
    val answers = Buffer[String]("Varmasti.", "Ei epäilystäkään.", "Ukkokin sanoisi niin.", "Bittini ennustavat niin.", "Kyllä, varmastikin.", "Voinet luottaa siihen.", "Näkökulmastani joo.", "Mitä todennäköisimmin.", "Siltä vaikuttaa.", "Kyllä.", "Niin minulle on kerrottu.",
      "Vastaus epävarma.", "Kysy myöhemmin uudestaan.", "En voi kertoa.", "Turvallisuusluokituksesti ei riitä vastauksen lukemiseen.", "Ennustuspiirit synkronoimatta.", "Keskity tarkemmin ja kysy uudestaan.", "Kenties",
      "Älä laske sen varaan.", "En luottaisi siihen.", "Ei.", "Lähteideni mukaan ei.", "Henget ovat epäsuotuisia.", "Epäilen.")
    val answer = answers((random.nextInt(answers.size)))
    sendMessage(out, answer, receivedFrom)
  }

  def bigRedButton(out: BufferedWriter, nick: String) = {
    sendMessage(out, "You shouldn't have done that, " + nick + ".", nick)
  }

  def terminate(out: BufferedWriter, nick: String) = {
    sendData(out, "MODE " + nick + " -o")
    sendMessage(out, "Nick " + nick + ", prepate to be terminated.", this.homeChannel)
    sendMessage(out, "Terminating in 3...", this.homeChannel)
    Thread.sleep(1000)
    sendMessage(out, "Terminating in 2...", this.homeChannel)
    Thread.sleep(1000)
    sendMessage(out, "Terminating in 1...", this.homeChannel)
    Thread.sleep(1000)
    sendMessage(out, "Hasta la vista, " + nick, this.homeChannel)
    sendData(out, "KICK " + this.homeChannel + " " + nick + " :You have been terminated.")
  }

  def relay(out: BufferedWriter, line: String) = {
    this.sendData(out, line.split("!relay ")(1))
  }

  def opme(out: BufferedWriter, nick: String) = {
    sendData(out, "MODE " + this.homeChannel + " " + nick + " +o")
  }

  def plannedFeatures(out: BufferedWriter, line: String, receivedFrom: String, nick: String) = {
    if (line.split("!planned ").lift(1).isDefined && line.split("!planned ")(1)(0) == 'g') {
      this.fileReader(out, if (this.isChannel) { this.recipient } else { this.homeChannel }, "plannedVersions.txt")
    } else {
      this.fileReader(out, nick, "plannedVersions.txt")
    }
  }

  def fileReader(out: BufferedWriter, receivedFrom: String, filename: String): Unit = {
    try {
      var lines = Vector[String]()
      this.bot.synchronized {
        val file = Source.fromFile(filename)
        lines = file.getLines.toVector
        file.close()
      }
      lines.foreach(sendMessage(out, _, receivedFrom))
    } catch {
      case nofile: FileNotFoundException => this.sendMessage(out, "Did not find file: " + filename, receivedFrom)
    }
  }

  def randomReader(out: BufferedWriter, receivedFrom: String, filename: String): Unit = {
    try {
      var lines = Vector[String]()
      this.bot.synchronized {
        val file = Source.fromFile(filename)
        lines = file.getLines.toVector
        file.close()
      }
      this.sendMessage(out, lines(Random.nextInt(lines.size)), receivedFrom)
    } catch {
      case nofile: FileNotFoundException => this.sendMessage(out, "Did not find file: " + filename, receivedFrom)
    }
  }

  def isChannel = {
    this.recipient(0) == '#'
  }

  def addToHistory(line: (Long, String)) = {
    if (this.messageHistory.size >= this.historySize) {
      this.messageHistory.dequeue()
    }
    this.messageHistory += line
  }

  def lastMessage = {
    this.messageHistory(this.messageHistory.size - 1)
  }

  def stats(out: BufferedWriter) = {
    this.sendMessage(out, "LATEBOT STATUS", this.recipient)
    this.sendMessage(out, "Current uptime: " + this.bot.convertTime(System.currentTimeMillis() - this.bot.startingTime), this.recipient)
    this.sendMessage(out, "Time since last scheduled maintenance: " + this.bot.convertTime(System.currentTimeMillis() - this.bot.lastCheck), this.recipient)
    this.sendMessage(out, "Running conversations at" + this.bot.conversations.keys.map(_.recipient).toVector.mkString(" ", ", ", "."), this.recipient)
    this.sendMessage(out, "Running a total of " + this.bot.conversations.keys.size + " threads", this.recipient)
    /*    if (!this.bot.blackList.keys.isEmpty) {
*      this.sendMessage(out, "Known troublemakers:" + this.bot.blackList.keys.map(_.nick).toVector.mkString(" ", ", ", "."), this.recipient)
*   }
*/ this.sendMessage(out, "All systems nominal", this.recipient)
  }

  def kill = {
    println("Killing thread at: " + this.recipient)
    this.stop = true
  }

  def quote(out: BufferedWriter, lineString: String) = this.bot.synchronized {
    val params = lineString.split("!quote ").lift(1).getOrElse("empty")
    if (params == "empty") {
      this.randomReader(out, this.address(lineString), "quotes.txt")
    } else {
      val nick = lineString.split(":")(1).split("!")(0)
      this.addQuote(out, lineString, nick, this.address(lineString), params)
    }
  }

  def addQuote(out: BufferedWriter, lineString: String, nick: String, receivedFrom: String, params: String) = {
    params(0) match {
      case '"' => this.textQuote(out, lineString, nick, receivedFrom, params)
      case 'N' => this.historyQuote(out, nick, receivedFrom, params)
      case _ => this.sendMessage(out, "Syntax error: start the quote with either \" or N.", receivedFrom)
    }
  }

  def textQuote(out: BufferedWriter, lineString: String, nick: String, receivedFrom: String, params: String) = {
    var quote = lineString.split("!quote ")(1).dropWhile(_ != '\"')
    val authorBlock = lineString.split("!quote ")(1).dropWhile(_ != '-')
    if (authorBlock == "-") {
      quote += "anonymous"
    }
    if (!authorBlock.lift(0).isDefined || !authorBlock.lift(1).isDefined || authorBlock.isEmpty) {
      this.sendMessage(out, "Syntax error: the quote must be entered as \"<quote>\" -<author>.", receivedFrom)
    } else if (this.alreadyQuoted(quote)) {
      this.sendMessage(out, "Quote already saved.", receivedFrom)
    } else if (!this.bot.conversations.keys.find(_.recipient == nick).isDefined) {
      val newQuery = this.bot.addConversation(nick, out)
      new Thread(newQuery).start()
      newQuery.addQuoteToConfirm(quote)
    } else {
      val targetQuery = this.bot.conversations.keys.find(_.recipient == nick).get
      targetQuery.synchronized {
        targetQuery.addQuoteToConfirm(quote)
        targetQuery.notify()
      }
    }
  }

  def historyQuote(out: BufferedWriter, nick: String, receivedFrom: String, params: String) = {
    val messageNumber: String = params.split("N").lift(1).getOrElse("-1").trim()
    if (messageNumber == "-1" || !this.stringToInt(messageNumber).isDefined || this.stringToInt(messageNumber).get > this.historySize) {
      this.sendMessage(out, "Syntax error: could not find the message.", receivedFrom)
    } else {
      val quoteString = this.messageHistory(this.messageHistory.size - 1 - messageNumber.toInt)._2
      val quote = quoteString.split(":").last
      val author = quoteString.split(":")(1).split("!")(0)
      if (this.alreadyQuoted("\"" + quote + "\"" + " -" + author)) {
        this.sendMessage(out, "Quote already saved.", receivedFrom)
      } else if (!this.bot.conversations.keys.find(_.recipient == nick).isDefined) {
        val newQuery = this.bot.addConversation(nick, out)
        new Thread(newQuery).start()
        newQuery.addQuoteToConfirm("\"" + quote + "\"" + " -" + author)
      } else {
        val targetQuery = this.bot.conversations.keys.find(_.recipient == nick).get
        targetQuery.synchronized {
          targetQuery.addQuoteToConfirm("\"" + quote + "\"" + " -" + author)
          targetQuery.notify()
        }
      }
    }
  }

  def stringToInt(s: String) = {
    try {
      Some(s.toInt)
    } catch {
      case e: NumberFormatException => None
    }
  }

  def alreadyQuoted(quote: String) = this.bot.synchronized {
    val file = Source.fromFile("quotes.txt")
    val lines = file.getLines().toVector
    file.close()
    lines.find(_ == quote).isDefined
  }

  def addQuoteToConfirm(quote: String) = {
    this.quoteToConfirm = quote
    this.savingQuote = true
  }

  def takeLine(line: (Long, String), nick: String): Unit

  def confirmQuote(quote: String): Unit

}
