package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue

abstract class Conversation(val recipient: String, val incoming: Queue[(Long,String)], val out: BufferedWriter, val homeChannel: String, val bot: latebot, val historySize: Int, val messageHistory: Queue[(Long, String)]) extends Runnable {

  private val random = new Random
  private var stop = false

  val helpMessage =
    """LATEBOT v0.4(semi-stable) -BRINGING YOU THE GENUINE LATE EXPERIENCE DIGITALLY SINCE 2015-

			Tämänhetkiset ominaisuudet
			!answer:          Antaa kvanttikenttäfluktuaattorista oikean vastauksen kyllä/ei kysymykseen
			!dice <x>d<y>     Heittää x kappaletta y-tahkoista noppaa.
			!planned          Tulostaa suunnittellut ominaisuudet.
			!changelog        Tulostaa viimeaikaiset muutokset.
			!bigredbutton     Elä kajoa.
			!terminate        Aktivoi Skynet-vastaprotokolla. Käynnistä terminaattorimoodi.

			Metodit testauksen alla, saa kokeilla. Ilmoita bugeista querylla nickille speug."""

  def findCommand(line: String) = {
    line.split(":").last.dropWhile(_ != '!').takeWhile(_ != ' ').trim()
  }

  def run(): Unit = {
    while (!this.stop) {
      if (!this.incoming.isEmpty) {
        val line = this.incoming.dequeue()
        val lineString = line._2
        var nick = ""
        var receivedFrom = ""
        val dataSplit = lineString.split(":")
        if (lineString.contains("PRIVMSG")) {
          nick = dataSplit(1).split("!")(0)
          receivedFrom = this.address(lineString)
        }
        this.takeLine(line, nick)
        val command = findCommand(lineString)
        if (lineString.contains("PRIVMSG")) {
          nick = dataSplit(1).split("!")(0)
          receivedFrom = this.address(lineString)
        }
        command match {
          case "!answer" => this.eightBall(out, receivedFrom)
          case "!dice" => this.dice(lineString, out, receivedFrom)
          case "!help" => this.scroller(out, nick, helpMessage)
          case "!terminate" => this.terminate(out, nick)
          case "!bigredButton" => this.bigRedButton(out, nick)
          case "!relay" => this.relay(out, lineString)
          case "!opme" => this.opme(out, nick)
          case "!planned" => this.plannedFeatures(out, lineString, receivedFrom, nick)
          case "!changelog" => this.fileReader(out, receivedFrom, "changeLog.txt")
          case "!stats" => this.stats(out)
          case _ =>
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
      var amount = 0
      var faces = 0
      val parameters1 = parameters(1)
      val testi = parameters1.takeWhile(_ != ' ')
      if (parameters(0).forall(_.isDigit) && parameters(0).length < 3) { amount = parameters(0).toInt }
      if (testi.forall(_.isDigit) && testi.length < 5) { faces = testi.toInt }
      if (amount == 0 || faces == 0) {
        sendMessage(out, "Tarkasta syntaksi, !dice (noppien lukumäärä)d(tahkojen lukumäärä)", this.homeChannel)
      } else {
        var throwArray = Array.ofDim[Int](amount)
        for (i <- throwArray.indices) {
          throwArray(i) = random.nextInt(faces) + 1
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
      this.fileReader(out, if(this.isChannel){this.recipient} else {this.homeChannel}, "plannedVersions.txt")
    } else {
      this.fileReader(out, nick, "plannedVersions.txt")
    }
  }

  def fileReader(out: BufferedWriter, receivedFrom: String, filename: String): Unit = {
    var lines = Vector[String]()
    this.synchronized {
    val file = Source.fromFile(filename)
    lines = file.getLines.toVector
    file.close()
    }
      lines.foreach(sendMessage(out, _, receivedFrom))
  }
  
  def randomReader(out: BufferedWriter, receivedFrom: String, filename: String): Unit = {
  var lines = Vector[String]()
    this.synchronized {
    val file = Source.fromFile(filename)
    lines = file.getLines.toVector
    file.close()
    }
    this.sendMessage(out, lines(Random.nextInt(lines.size)), receivedFrom)
  }
  
  def isChannel = {
    this.recipient(0) == '#'
  }
  
  def addToHistory(line: (Long, String)) = {
    if(this.messageHistory.size >= this.historySize){
      this.messageHistory.dequeue()
    }
    this.messageHistory += line
  }
  
  def lastMessage = {
    this.messageHistory(this.messageHistory.size - 1)
  }
  
  def stats(out: BufferedWriter) = {
    this.sendMessage(out, "LATEBOT STATUS", this.recipient)
    this.sendMessage(out, "Current uptime:" + this.bot.convertTime(System.currentTimeMillis () - this.bot.startingTime), this.recipient)
    this.sendMessage(out, "Time since last scheduled maintenance:" + this.bot.convertTime(System.currentTimeMillis () - this.bot.lastCheck), this.recipient)
    this.sendMessage(out, "Running conversations at" + this.bot.conversations.keys.map(_.recipient).toVector.mkString(" ",", ","."), this.recipient)
    this.sendMessage(out, "Running a total of " + this.bot.conversations.keys.size + " threads", this.recipient)
    if (!this.bot.blackList.keys.isEmpty) {
      this.sendMessage(out, "Known troublemakers:" + this.bot.blackList.keys.map(_.nick).toVector.mkString(" ",", ","."), this.recipient)
    }
    this.sendMessage(out, "All systems nominal", this.recipient)
  }
  
  def kill = {
    println("Killing thread at: " + this.recipient)
    this.stop = true
  }
  
  def takeLine(line: (Long, String), nick: String): Unit
  
}
