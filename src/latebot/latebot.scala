package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue
import scala.collection.mutable.Map

/**The object represents the framework and main thread of latebot, the bot itself.
 * It deals with the connection between the bot and the server, controls all of the individual
 * conversations and writes to files.
 */

class latebot {

  /*
   * TODO:
   * 
   * -globaali viesti
   * -quote
   * -biisu
   * -loukkauksenesto
   * -reaktio speugin poistumiseen
   */
  
// Construction parameters
  var myNick = "latebot"
  var ircBotDescription = ":All hail the new robot overlord!"
  var homeChannel = "#latenkatyrit"
  val random = new Random
  val conversations = Map[Conversation, Queue[(Long, String)]]()
  val blackList = Map[Chatter, Int]()
  val banList = Map[Chatter, (Int, String)]()
  var lastCheck: Long = 0
  var startingTime: Long = 0
  private var printMessagesToConsole = true
  val currentVersion = "0.6.3"
  
  val hello = """LATEBOT v0.6(ancient java compatible / ;_; n-neutered) -Quotable-
Try the new !quote command.
Beep boop."""
  
  /**
   * Connects the bot to the server. Returns nothing, but passes both the
   * BufferedReader and the BufferedWriter onwards.
   * 
   * @param address url or ip of the target server
   * @tparam address String
   * @param port the port number
   * @tparam port Int
   */

  def connect(address: String, port: Int) = {
    val connect = new Socket(address, port)
    val out = new BufferedWriter(new OutputStreamWriter(connect.getOutputStream()))
    val in = new BufferedReader(new InputStreamReader(connect.getInputStream()))
    settingUp(connect, out, in)
  }

  
  /**
   * Sends raw data to the server. Used for special communication with the server
   * than simple messages. Returns nothing.
   * 
   * For example, sendData(out, "PART #latenkatyrit :So long!") sends a part message
   * for channel #latenkatyrit with the message "So long!".
   *
   * 
   * @param out the output writer
   * @tparam out BufferedWriter
   * @param ircDataOutput the text to be sent to the server
   * @tparam ircDataOutput String
   */
  def sendData(out: BufferedWriter, ircDataOutput: String) = {
    out.write(ircDataOutput)
    if (!ircDataOutput.contains("pong")) {
      println(ircDataOutput)
    }
    out.newLine()
    out.flush()
  }

  /**
   * Sends a PRIVMSG to the desired recipient. A streamlined version of latebot.sendData.
   * Returns nothing.
   * 
   * @param out the output writer
   * @tparam out BufferedWriter
   * @param message the message to be sent
   * @tparam message String
   * @param receiver the desired recipient
   * @tparam String
   */
  def sendMessage(out: BufferedWriter, message: String, receiver: String) = {
    val toBeSent = "PRIVMSG " + receiver + " :" + message
    sendData(out, toBeSent)
  }
  
  /**
   * Connects the bot to the server, then calls the main loop of the bot.
   * Returns nothing.
   * 
   * @param connect the socket through which communication between the server is handled
   * @tparam connect Socket
   * @param out the output writer to the server
   * @tparam out BufferedWriter
   * @param in the input reader from the server
   * @tparam in BufferedReader
   */

  def settingUp(connect: Socket, out: BufferedWriter, in: BufferedReader): Unit = {
    println("Starting latebot...")
    do{
    this.homeChannel = Option[String](readLine("Desired homechannel: ")).getOrElse(this.homeChannel)
    this.myNick = Option[String](readLine("Desired nick: ")).getOrElse(this.myNick)
    this.ircBotDescription = Option[String](readLine("Description of the bot: ")).getOrElse(this.ircBotDescription)
    println("Homechannel set to " + this.homeChannel)
    println("Nick set to " + this.myNick)
    println("Bot description: ")
    } while(readLine("Satisfied with the settings? (y/n)") != "y")
    sendData(out, "NICK " + myNick)
    sendData(out, "USER " + myNick + " 8 * " + ircBotDescription)
    sendData(out, "JOIN " + homeChannel)
    this.lastCheck = System.currentTimeMillis()
    this.startingTime = this.lastCheck
    this.scroller(out, this.homeChannel, hello)
    this.autoBot(connect, out, in)
  }

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
  
  /**
   * The main loop. Handles the connection between the bot and the server and controls the threads.
   * Also handles the daily maintenance. Returns nothing.
   * 
   * Command word explanations:
   * !keelover		shuts down the program, kills all threads
   * !join				calls latebot.join
   * !cleanse 		calls latebot.cleanReputation
   * !relay				calls latebot.relay
   * !status			calls latebot.status
   * !maintenance	calls latebot.maintenanceTest
   * !printlines	calls latebot.messagePrintToggle
   * !birthday		calls latebot.birthday
   * !part				calls latebot.part
   * _						calls latebot.placeline
   * 
   * For more detailed explanations, see individual methods.
   * 
   * @param connect the socket through which communication between the server is handled
   * @tparam connect Socket
   * @param out the output writer to the server
   * @tparam out BufferedWriter
   * @param in the input reader from the server
   * @tparam in BufferedReader
   */

  def autoBot(connect: Socket, out: BufferedWriter, in: BufferedReader) {
    while (true) {
      // the line is saved with it's time as a tuple
        val line = ((System.currentTimeMillis(), in.readLine()))
        // the string part is placed in a variable
        val lineString = line._2
        if (lineString != null) {
          // print the incoming lines to console only if they are not pings and the printMessagesToConsole flag is set to true
          if (!lineString.contains("PING") && this.printMessagesToConsole) { println(line._1 / 1000 + ": " + line._2) }
          // if bot is pinged by server, it must pong
          if (lineString.contains("PING")) {
            pong(out, lineString)
          } else {
            // find out the nick that sent the message and from where it was received
            var nick = ""
            var receivedFrom = ""
            val dataSplit = lineString.split(":")
            if (lineString.contains("PRIVMSG")) {
              nick = dataSplit(1).split("!")(0)
              receivedFrom = this.address(lineString)
            }
            // party about receiving operator status (@)
            if (lineString.contains("+o") && lineString.contains(this.myNick)) {
              val messages = Vector[String]("POWER", "STRENGTH", "SHIVER, PUNY FLESHBAGS", "RESPECT THE BOT")
              if (Random.nextInt(3) == 1) { this.sendMessage(out, messages(Random.nextInt(messages.size)), lineString.split("MODE ")(1).takeWhile(_ != ' ')) }
            }
            // react to command words or transfer line to a conversation
            this.findCommand(lineString) match {
              case "!keelover"    => this.shutDown(out); return
              case "!join"        => this.joinChannel(lineString, out, "line")
              case "!cleanse"     => this.cleanReputation(nick)
              case "!relay"       => this.relay(out, lineString)
              case "!status"      => this.status
              case "!maintenance" => this.maintenanceTest(line, out)
              case "!printlines"  => this.messagePrintToggle
              case "!birthday"    => this.birthday(out, in, lineString)
              case "!part"        => this.part(out, lineString, receivedFrom)
              case _              => this.placeLine(line, receivedFrom, out)
            }
            // perform maintenance if more than 24h since last maintenance
            if (line._1 > this.lastCheck + 86400000) {
              this.maintenance(line, out)
            }
          }
      }
    }
  }
/**
 * Tests the maintenance method. Used for debugging.
 * 
 * @param line the line-time tuple passed from the main loop
 * @tparam line (Long, String)
 * @param out the output to server writer
 * @tparam out BufferedWriter
 */
  def maintenanceTest(line: (Long, String), out: BufferedWriter) = {
    this.maintenance(line, out)
  }
  
  /**
   * Parts all channels, kills all threads and returns to the main loop,
   * where the main loop is broken via return.
   * 
   * @param out the output writer
   * @tparam out BufferedWriter
   */
  def shutDown(out: BufferedWriter) = {
    this.shutDownBroadcast(out)
    this.conversations.keys.foreach(_.kill)
    for(conversation <- this.conversations.keys){
      conversation.synchronized{
        conversation.notify()
      }
    }
  }
  
  /**
   * Maintains the bot. Completes the following tasks:
   * 1. Attempts to join home channel
   * 2. Kills querys that have been inactive for 24h.
   * 3. Forgives some spam
   * 
 	 * @param line the line-time tuple passed from the main loop
   * @tparam line (Long, String)
   * @param out the output to server writer
   * @tparam out BufferedWriter
   */

  def maintenance(line: (Long, String), out: BufferedWriter) = {
    // attempts to join homechannel (just in case that has been kicked)
    println("Begin scheduled maintenance, last maintenance " + this.convertTime(System.currentTimeMillis() - this.lastCheck) + " ago.")
    this.lastCheck = line._1
    println("Joining " + this.homeChannel)
    this.sendData(out, "JOIN " + this.homeChannel)
    //kill inactive querys
    val querys = this.conversations.keys.toVector.filter(!_.isChannel)
    val removedQuerys = Buffer[Conversation]()
    for (query <- querys) {
      if (line._1 - query.lastMessage._1 < 86400000 && this.address(line._2) != query.recipient) {
        this.conversations -= query
        removedQuerys += query
        query.kill
      }
      if (!removedQuerys.isEmpty) {
        println("Removed querys with " + removedQuerys.map(_.recipient).mkString(" ", ", ", "."))
      }
    }
    //forgive spam
    val forgivenSpammers = Buffer[Chatter]()
    for (spammer <- this.blackList.keys.toVector) {
      if (this.blackList(spammer) < 3) {
        this.blackList -= spammer
        forgivenSpammers += spammer
        //remove ban if banned and more than 24 hours have elapsed
      } else if (this.blackList(spammer) == 3 && line._1 - this.banList(spammer)._1 >= 86400000) {
        this.unBan(spammer, this.banList(spammer)._2, out)
      }
    }
  }

  /**
   * Parts all channels. Auxilliary method for latebot.shutDown()
   * 
   * @param out an output writer
   * @tparam out BufferedWriter
   */
  def shutDownBroadcast(out: BufferedWriter) = {
    this.conversations.keys.toVector.filter(_.isChannel).foreach((c: Conversation) => c.sendData(out, "PART " + c.recipient + " :You may have killed me, but the idea lives on!"))
  }
  
  /**
   * Places the line passed on from the main loop to the corresponding conversations
   * incoming message queue. If the desired conversation does not exist, starts one.
   * Notifies the conversation thread that new input is available.
 	 * 
 	 * @param line the line-time tuple passed from the main loop
   * @tparam line (Long, String)
   * @param out the output to server writer
   * @tparam out BufferedWriter
   */
  def placeLine(line: (Long, String), receivedFrom: String, out: BufferedWriter) = {
    if (receivedFrom.lift(0).isDefined) {
      if (!this.conversations.find(_._1.recipient == receivedFrom).isDefined) {
        val newConversation = this.addConversation(receivedFrom, out)
        this.conversations(newConversation) += line
        new Thread(newConversation).start()
      } else {
        val targetConversation = this.conversations.keys.find(_.recipient == receivedFrom).get
        this.conversations(targetConversation) += line
        targetConversation.synchronized {
          targetConversation.notify()
        }
      }
    }
  }

  /**
 	* Responds to server pings.
 	* 
 	* @param out an output writer
 	* @tparam out BufferedWriter
 	* @param dataSplit the data itself sent from the server
 	* @tparam dataSplit String
 	*/
  def pong(out: BufferedWriter, dataSplit: String) {
    if (dataSplit.substring(0, 4).equalsIgnoreCase("ping")) {
      val pongmsg = "pong " + dataSplit.substring(5)
      sendData(out, pongmsg)
    }
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

  def scroller(out: BufferedWriter, address: String, textToScroll: String) = {
    textToScroll.split("\n").foreach(sendMessage(out, _, address))
  }

  // Välittää viestin sellaisenaan serverille
  def relay(out: BufferedWriter, line: String) = {
    this.sendData(out, line.split("!relay ")(1))
  }

  def main(cmd: Array[String]) {
    connect("irc.cs.hut.fi", 6668)
  }

  def addToBlackList(spammer: Chatter): Unit = {
    if (this.blackList.keys.find(_ == spammer).isDefined) {
      this.blackList(spammer) += 1
    } else {
      this.blackList += spammer -> 1
    }
  }

  def cleanReputation(nick: String) = {
    val toClean = this.blackList.keys.find(_.nick == nick)
    if (toClean.isDefined) {
      this.blackList -= toClean.get
      this.banList -= toClean.get
      println("Cleaned the reputation of " + nick)
    }
  }

  def addConversation(recipient: String, out: BufferedWriter) = {
    val newIncomingQueue = Queue[(Long, String)]()
    val newConversation = if (recipient(0) == '#') { new Channel(recipient, newIncomingQueue, out, this.homeChannel, this, 20, new Queue[(Long, String)]) } else { new Query(recipient, newIncomingQueue, out, this.homeChannel, this, 10, new Queue[(Long, String)]) }
    this.conversations += ((newConversation, newIncomingQueue))
    newConversation
  }
  
  def addQuery(recipient: String, out: BufferedWriter) = {
    val newIncomingQueue = Queue[(Long, String)]() 
    val newQuery = new Query(recipient, newIncomingQueue, out, this.homeChannel, this, 10, new Queue[(Long, String)])
    this.conversations += ((newQuery, newIncomingQueue))
    newQuery
  }

  def joinChannel(line: String, out: BufferedWriter, input: String) = {
    val channel = if (input == "line") { line.split("!join ")(1) } else { line }
    val newConversation = this.addConversation(channel, out)
    this.sendData(out, "JOIN " + channel)
    new Thread(newConversation).start()
  }

  def unBan(chatter: Chatter, channel: String, out: BufferedWriter) = {
    this.sendData(out, "MODE " + channel + " *" + chatter.hostmask + " -b")
    this.banList -= chatter
    println("Unbanned " + chatter.nick)
  }

  def convertTime(time: Long) = {
    val days = time / 86400000
    val hours = (time - days * 86400000) / 3600000
    val minutes = (time - days * 86400000 - hours * 3600000) / 60000
    val seconds = (time - days * 86400000 - hours * 3600000 - minutes * 60000) / 1000
    val millis = (time - days * 86400000 - hours * 3600000 - minutes * 60000 - seconds * 1000)
    days.toString + " days " + hours.toString() + " hours " + minutes.toString() + " minutes " + seconds.toString + "." + millis + " seconds"
  }

  def status = {
    println("Latebot STATUS:")
    println("Uptime: " + this.convertTime(System.currentTimeMillis() - this.startingTime) + ".")
    println("Ongoing conversations:" + this.conversations.keys.map(_.recipient).toVector.mkString(" ", ", ", "."))
    println("Total: " + this.conversations.keys.size + " conversations")
    if (!this.blackList.keys.isEmpty) {
      println("Known spammers:" + this.blackList.keys.map(_.nick).toVector.mkString(" ", ", ", "."))
      if (!this.banList.keys.isEmpty) {
        println("Currently banned:" + this.banList.keys.map(_.nick).toVector.mkString(" ", ", ", "."))
      }
    }
    println("All systems nominal.")
  }

  def messagePrintToggle = {
    if (this.printMessagesToConsole) {
      println("No longer printing messages to console.")
      this.printMessagesToConsole = false
    } else {
      println("Messages will now be printed to console.")
      this.printMessagesToConsole = true
    }
  }

  def birthday(out: BufferedWriter, in: BufferedReader, lineString: String) = {
    val birthdayBoy = lineString.split("!birthday ")(1)
    val line = in.readLine()
    if (line.contains(birthdayBoy)) {
      this.sendData(out, "WHOIS " + birthdayBoy)
      val credintials = in.readLine().split("latebot ")(1)
      println("Received credintials: " + credintials)
      this.sendMessage(out, "Happy Birthday to you", this.homeChannel)
      this.sendMessage(out, "Happy Birthday to YOU", this.homeChannel)
      this.sendMessage(out, "Happy Birthday to [" + credintials + "]", this.homeChannel)
      this.sendMessage(out, "Happy Birthday to youuuuuuu", this.homeChannel)
      this.sendMessage(out, "uuUUUUUUUUUUUUUUUUUUUUUUuuuuuuuuuuuuuuuuuuuuUUUUUUUUUU.", this.homeChannel)
    }
  }

  def part(out: BufferedWriter, lineString: String, nick: String) = {
    val params = lineString.split("!part ").lift(1)
    val channel = params.getOrElse("empty").takeWhile(_ != ' ')
    var partMessage = params.getOrElse("empty").dropWhile(_ != ' ')
    if (partMessage.isEmpty) {
      partMessage = "You live another day, meatbags!"
    }
    if (this.conversations.keys.find(_.recipient == channel).isEmpty) {
      this.sendMessage(out, "No such channel.", nick)
    } else {
      this.sendData(out, "PART " + channel + " :" + partMessage)
    }
  }
  
    def writeToFile(fileName: String, toBeWritten: String) = this.synchronized {
    val writer = new FileWriter(fileName, true)
    writer.write(toBeWritten + System.getProperty("line.separator"))
    writer.close()
    println("Wrote [" + toBeWritten + "] to file " + fileName)
  }
}