package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue
import scala.util.Try

abstract class Conversation(val recipient: String, val incoming: Queue[Message], val out: BufferedWriter, val homeChannel: String, val bot: latebot, val historySize: Int, val messageHistory: Queue[Message]) extends Runnable {

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
!weather          Tulostaa viimeisimmän Otaniemen säätiedon

Metodit testauksen alla, saa kokeilla. Ilmoita bugeista querylla nickille speug."""

  def run(): Unit = {
    while (!this.stop) {
      if (!this.incoming.isEmpty && !this.savingQuote) {
        val msg = this.incoming.dequeue()
        val command = msg.command
        if (msg.type == "PRIVMSG") {
          this.takeLine(msg)
        }
        command match {
          case Some("!answer")       => this.eightBall(out, msg.address)
          case Some("!dice")         => this.dice(msg, out)
          case Some("!help")         => this.scroller(out, msg, helpMessage)
          case Some("!terminate")    => this.terminate(out, msg)
          case Some("!bigredButton") => this.bigRedButton(out, msg)
          case Some("!relay")        => this.relay(out, msg)
          case Some("!opme")         => this.opme(out, msg)
          case Some("!planned")      => this.plannedFeatures(out, msg)
          case Some("!changelog")    => this.fileReader(out, msg.address, "changeLog.txt")
          case Some("!stats")        => this.stats(out)
          case Some("!quote")        => this.quote(out, msg.raw)
	        case Some("!weather")      => this.weather(out, msg.address)
          case None                  =>
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

  def address(msg: Message): String = {
    var recipient = line.split("PRIVMSG ")(1).takeWhile(_ != ' ')
    if (recipient(0) == '#' || recipient(0) == '!') {
      recipient
    } else {
      recipient = line.split(":")(1).split("!")(0)
      recipient
    }
  }

  def dice(msg: Message, out: BufferedWriter) {
    if (msg.raw.split("!dice ").size >= 2) {
      val parameters = msg.raw.split("!dice ")(1).split('d')
      var amount = parameters.lift(0).getOrElse("0").trim()
      var faces = parameters.lift(1).getOrElse("0").trim()
      if (amount.forall(_.isDigit) && amount.length < 3 && !amount.isEmpty()) { amount = amount } else { amount = "0" }
      if (faces.forall(_.isDigit) && faces.length < 5 && !faces.isEmpty()) { faces = faces } else { faces = "0" }
      if (amount == "0" || faces == "0") {
        sendMessage(out, "Tarkasta syntaksi, !dice (noppien lukumäärä)d(tahkojen lukumäärä)", msg.address)
      } else {
        var throwArray = Array.ofDim[Int](amount.toInt)
        for (i <- throwArray.indices) {
          throwArray(i) = random.nextInt(faces.toInt) + 1
        }
        val total = throwArray.sum
        val message = "Heitit " + throwArray.mkString(" + ") + " = " + total + "!"
        sendMessage(out, message, msg.address)
      }
    } else {
      sendMessage(out, "Unohtuivatko parametrit?", msg.address)
    }
  }

  def scroller(out: BufferedWriter, address: String, textToScroll: String) = {
    textToScroll.split("\n").foreach(sendMessage(out, _, address))
  }

  def eightBall(out: BufferedWriter, address: String) = {
    val answers = Buffer[String]("Varmasti.", "Ei epäilystäkään.", "Ukkokin sanoisi niin.", "Bittini ennustavat niin.", "Kyllä, varmastikin.", "Voinet luottaa siihen.", "Näkökulmastani joo.", "Mitä todennäköisimmin.", "Siltä vaikuttaa.", "Kyllä.", "Niin minulle on kerrottu.",
      "Vastaus epävarma.", "Kysy myöhemmin uudestaan.", "En voi kertoa.", "Turvallisuusluokituksesti ei riitä vastauksen lukemiseen.", "Ennustuspiirit synkronoimatta.", "Keskity tarkemmin ja kysy uudestaan.", "Kenties",
      "Älä laske sen varaan.", "En luottaisi siihen.", "Ei.", "Lähteideni mukaan ei.", "Henget ovat epäsuotuisia.", "Epäilen.")
    val answer = answers((random.nextInt(answers.size)))
    sendMessage(out, answer, address)
  }

  def bigRedButton(out: BufferedWriter, msg: Message) = {
    sendMessage(out, "You shouldn't have done that, " + msg.nick.get + ".", msg.nick.get)
  }

  def terminate(out: BufferedWriter, msg: Message) = {
    val nick = msg.nick.get
    sendData(out, "MODE " + this.homeChannel + " -o " + nick)
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

  def relay(out: BufferedWriter, msg: Message) = {
    this.sendData(out, msg.raw.split("!relay ")(1))
  }

  def opme(out: BufferedWriter, msg: Message) = {
    sendData(out, "MODE " + this.homeChannel + " +o " + msg.nick.get)
  }

  def plannedFeatures(out: BufferedWriter, msg: Message) = {
    if (msg.raw.split("!planned ").lift(1).isDefined && msg.raw.split("!planned ")(1)(0) == 'g') {
      this.fileReader(out, if (this.isChannel) { this.recipient } else { this.homeChannel }, "plannedVersions.txt")
    } else {
      this.fileReader(out, msg.nick.get, "plannedVersions.txt")
    }
  }

  def fileReader(out: BufferedWriter, address: String, filename: String): Unit = {
    try {
      var lines = Vector[String]()
      this.bot.synchronized {
        val file = Source.fromFile(filename)
        lines = file.getLines.toVector
        file.close()
      }
      lines.foreach(sendMessage(out, _, address))
    } catch {
      case nofile: FileNotFoundException => this.sendMessage(out, "Did not find file: " + filename, address)
    }
  }

  def randomReader(out: BufferedWriter, address: String, filename: String): Unit = {
    try {
      var lines = Vector[String]()
      this.bot.synchronized {
        val file = Source.fromFile(filename)
        lines = file.getLines.toVector
        file.close()
      }
      this.sendMessage(out, lines(Random.nextInt(lines.size)), address)
    } catch {
      case nofile: FileNotFoundException => this.sendMessage(out, "Did not find file: " + filename, address)
    }
  }

  def isChannel = {
    (this.recipient(0) == '#') || (this.recipient(0) == '!')
  }

  def addToHistory(msg: Message) = {
    if (this.messageHistory.size >= this.historySize) {
      this.messageHistory.dequeue()
    }
    this.messageHistory += msg
  }

  def lastMessage: Message = {
    this.messageHistory.last
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

  def quote(out: BufferedWriter, msg: Message) = this.bot.synchronized {
    val params = msg.raw.split("!quote ").lift(1).getOrElse("empty")
    if (params == "empty") {
      this.randomReader(out, msg.address, "quotes.txt")
    } else {
      this.addQuote(out, msg, params)
    }
  }

  def addQuote(out: BufferedWriter, msg: Message, params: String) = {
    params(0) match {
      case '"' => this.textQuote(out, msg, params)
      case 'N' => this.historyQuote(out, msg, params)
      case _ => this.sendMessage(out, "Syntax error: start the quote with either \" or N.", msg.address)
    }
  }

  def textQuote(out: BufferedWriter, msg: Message, params: String) = {
    var quote = msg.raw.split("!quote ")(1).dropWhile(_ != '\"')
    val authorBlock = msg.raw.split("!quote ")(1).dropWhile(_ != '-')
    if (authorBlock == "-") {
      quote += "anonymous"
    }
    if (!authorBlock.lift(0).isDefined || !authorBlock.lift(1).isDefined || authorBlock.isEmpty) {
      this.sendMessage(out, "Syntax error: the quote must be entered as \"<quote>\" -<author>.", msg.address)
    } else if (this.alreadyQuoted(quote)) {
      this.sendMessage(out, "Quote already saved.", msg.address)
    } else if (!this.bot.conversations.keys.find(_.recipient == msg.nick.get).isDefined) {
      val newQuery = this.bot.addConversation(msg.nick.get, out)
      new Thread(newQuery).start()
      newQuery.addQuoteToConfirm(quote)
    } else {
      val targetQuery = this.bot.conversations.keys.find(_.recipient == msg.nick.get).get
      targetQuery.synchronized {
        targetQuery.addQuoteToConfirm(quote)
        targetQuery.notify()
      }
    }
  }

  def historyQuote(out: BufferedWriter, msg: Message, params: String) = {
    val messageNumber: String = params.split("N").lift(1).getOrElse("-1").trim()
    if (messageNumber == "-1" || !this.stringToInt(messageNumber).isDefined || this.stringToInt(messageNumber).get > this.historySize) {
      this.sendMessage(out, "Syntax error: could not find the message.", msg.address)
    } else {
      val quoteString = this.messageHistory(this.messageHistory.size - 1 - messageNumber.toInt)._2
      val quote = quoteString.split(":").last
      val author = quoteString.split(":")(1).split("!")(0)
      if (this.alreadyQuoted("\"" + quote + "\"" + " -" + author)) {
        this.sendMessage(out, "Quote already saved.", msg.address)
      } else if (!this.bot.conversations.keys.find(_.recipient == msg.nick.get).isDefined) {
        val newQuery = this.bot.addConversation(msg.nick.get, out)
        new Thread(newQuery).start()
        newQuery.addQuoteToConfirm("\"" + quote + "\"" + " -" + author)
      } else {
        val targetQuery = this.bot.conversations.keys.find(_.recipient == nick.get).get
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

  def takeLine(line: (Long, String), msg: Message): Unit

  def confirmQuote(quote: String): Unit

  def weather(out: BufferedWriter, msg: Message) {
    val raw = Source.fromURL("http://outside.aalto.fi/current.txt").mkString
    val output = "Current weather in Otaniemi: " + raw.split(": ").lift(1).getOrElse("Could not fetch weather data")
    this.sendMessage(out,output,msg.address)
  }
}
