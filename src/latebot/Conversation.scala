package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue

abstract class Conversation(val recipent: String, val incoming: Queue[String], val out: BufferedWriter, val homeChannel: String) extends Runnable {
  
  val random = new Random
  
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

  def run(): Unit = {
    val line = this.incoming.dequeue()
        if (!line.contains("PING")) { println(line) }
        var nick = ""
        var receivedFrom = ""
        val dataSplit = line.split(":")
        if (line.contains("PRIVMSG")) {
          nick = dataSplit(1).split("!")(0)
          receivedFrom = this.address(line)
        } else if (line.contains("!answer")) {
            val message = this.eightBall
            sendMessage(out, message, receivedFrom)
        } else if (line.contains("!dice")) {
            this.dice(line, out, receivedFrom)
        } else if (line.contains("!keelover")) {
            sendData(out, "PART " + this.homeChannel + " :You may have killed me, but the idea lives on!")
            return
        } else if (line.contains("!help")) {
            this.scroller(out, nick, helpMessage)
        } else if (line.contains("!terminate")) {
            this.terminate(out, dataSplit)
        } else if (line.contains("!bigredbutton")) {
            val nick = dataSplit(1).split("!")(0)
            sendMessage(out, "You shouldn't have done that, " + nick + ".", nick)
        } else if (line.contains('?')) {
            if (random.nextInt(50) == 0) {
            sendMessage(out, this.eightBall, receivedFrom)
          }
        } else if (line.contains("!relay")) {
            this.relay(out, line)
        } else if (line.contains("!opme")) {
          sendData(out, "MODE " + this.homeChannel + " " + nick + " +o")
        } else if (line.contains("!planned")) {
          if (line.split("!planned ")(1)(0) == 'g') {
            this.fileReader(out, this.homeChannel, "plannedVersions.txt")
          } else {
              this.fileReader(out, nick, "plannedVersions.txt")
          }
        } else if (line.contains("!changelog")) {
            this.fileReader(out, nick, "changeLog.txt")
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
  }

def address(line: String): String = {
    var recipent = line.split("PRIVMSG ")(1).takeWhile(_ != ' ')
    if (recipent(0) == '#') {
      recipent
    } else {
      recipent = line.split(":")(1).split("!")(0)
      recipent
    }
  }

  def dice(line: String, out: BufferedWriter, receiver: String) {
    if(line.split("!dice ").size >= 2){
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

  def eightBall = {
    val vastaukset = Buffer[String]("Varmasti.", "Ei epäilystäkään.", "Ukkokin sanoisi niin.", "Bittini ennustavat niin.", "Kyllä, varmastikin.", "Voinet luottaa siihen.", "Näkökulmastani joo.", "Mitä todennäköisimmin.", "Siltä vaikuttaa.", "Kyllä.", "Niin minulle on kerrottu.",
      "Vastaus epävarma.", "Kysy myöhemmin uudestaan.", "En voi kertoa.", "Turvallisuusluokituksesti ei riitä vastauksen lukemiseen.", "Ennustuspiirit synkronoimatta.", "Keskity tarkemmin ja kysy uudestaan.", "Kenties",
      "Älä laske sen varaan.", "En luottaisi siihen.", "Ei.", "Lähteideni mukaan ei.", "Henget ovat epäsuotuisia.", "Epäilen.")
    vastaukset(random.nextInt(vastaukset.size))
  }

  def terminate(out: BufferedWriter, dataSplit: Array[String]) = {
    val nick = dataSplit(1).split("!")(0)
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

  def fileReader(out: BufferedWriter, receivedFrom: String, filename: String):Unit = synchronized {
    val file = Source.fromFile(filename)
    val lines = file.getLines.toVector
    try {
      lines.foreach(sendMessage(out, _, receivedFrom))
    } finally {
      file.close()
    }
  }
}
