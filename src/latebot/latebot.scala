package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue
import scala.collection.mutable.Map

/**
 * The object represents the framework and main thread of latebot, the bot itself.
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
  var homeChannel = "!latenkatyrit"
  val random = new Random
  val conversations = Map[Conversation, Queue[Message]]()
  val tutorialModeConversations = Buffer[String]()
  val blackList = Map[Chatter, Int]()
  val banList = Map[Chatter, (Int, String)]()
  var lastCheck: Long = 0
  var startingTime: Long = 0
  private var printMessagesToConsole = true
  val currentVersion = "1.2"

  val hello = """LATEBOT v1.2(Release build) -Quotable-
Source at https://github.com/speug/latebot
More quotes! Terminator mode! Weather!
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
   * Sends raw data to the server. Used for more special communication with the server
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
   * Returns nothing. Allows the user to choose home channel, nick and description.
   * If no input is given, uses the hardcoded default options.
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
    do {
      val newChannel = readLine("Desired homechannel: ")
      this.homeChannel = if(!newChannel.isEmpty){ newChannel} else {this.homeChannel}
      val newNick = readLine("Desired nick: ")
      this.myNick = if(!newNick.isEmpty){newNick} else {this.myNick}
      val newDescription = readLine("Description of the bot: ")
      this.ircBotDescription = if(!newDescription.isEmpty){newDescription} else {this.ircBotDescription}
      println("Homechannel set to " + this.homeChannel)
      println("Nick set to " + this.myNick)
      println("Bot description: " + ircBotDescription)
    } while (readLine("Satisfied with the settings? (y/n)") != "y")
    sendData(out, "NICK " + myNick)
    sendData(out, "USER " + myNick + " 8 * " + ":" + ircBotDescription)
    sendData(out, "JOIN " + homeChannel)
    this.lastCheck = System.currentTimeMillis()
    this.startingTime = this.lastCheck
    this.scroller(out, this.homeChannel, hello)
    this.autoBot(connect, out, in)
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
      // a new message object is created
      val msg = new Message(in.readLine())
      if (msg.raw != null) {
        // print the incoming lines to console only if they are not pings and the printMessagesToConsole flag is set to true
        if (msg.type == "PING" && this.printMessagesToConsole) { println(msg.time + ": " + msg.raw) }
        // if bot is pinged by server, it must pong
        if (msg.type == "PING") {
          pong(out, msg)
        } else if(msg.type == "PRIVMSG"){
          // react to command words or transfer line to a conversation
          msg.commang match {
            case Some("!keelover") =>
              this.shutDown(out,"Shutting down."); return
            case Some("!join")            => this.joinChannel(msg, out, "line")
            case Some("!cleanse")         => this.cleanReputation(msg.nick.get)
            case Some("!relay")           => this.relay(out, msg)
            case Some("!status")          => this.status
            case Some("!maintenance")     => this.maintenanceTest(msg, out)
            case Some("!printlines")      => this.messagePrintToggle
            case Some("!birthday")        => this.birthday(out, in, msg)
            case Some("!part")            => this.part(out, msg)
            case Some("!addtutorial")     => this.addTutorialModeChannel(msg, out)
            case Some("!removetutorial")  => this.removeTutorialModeChannel(msg, out)
	          case Some("!update")          => this.shutDown(out,"Updating."); return
            case None                     => this.placeLine(msg, out)

          }
          // perform maintenance if more than 24h since last maintenance
          if (msg.ms > this.lastCheck + 86400000) {
            this.maintenance(msg, out)
          }
          }
          // party about receiving operator status (@)
          if (msg.raw.contains("+o " + this.myNick)) {
            val messages = Vector[String]("POWER", "STRENGTH", "SHIVER, PUNY FLESHBAGS", "RESPECT THE BOT")
            if (Random.nextInt(3) == 1) { this.sendMessage(out, messages(Random.nextInt(messages.size)), msg.raw.split("MODE ")(1).takeWhile(_ != ' ')) }
          }
          if (msg.type == "JOIN") {
            this.helpNewUser(msg, out)
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
  def maintenanceTest(msg: Message, out: BufferedWriter) = {
    this.maintenance(msg, out)
  }

  /**
   * Parts all channels, kills all threads and returns to the main loop,
   * where the main loop is broken via return.
   *
   * @param out the output writer
   * @tparam out BufferedWriter
   */
  def shutDown(out: BufferedWriter, prtMsg: String) = {
    this.shutDownBroadcast(out, prtMsg)
    this.conversations.keys.foreach(_.kill)
    for (conversation <- this.conversations.keys) {
      conversation.synchronized {
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

  def maintenance(msg: Message, out: BufferedWriter) = {
    val sender = msg.address
    // attempts to join homechannel (just in case that has been kicked)
    println("Begin scheduled maintenance, last maintenance " + this.convertTime(System.currentTimeMillis() - this.lastCheck) + " ago.")
    println("Maintenance trigger message: " + msg.raw)
    this.lastCheck = msg.ms
    println("Joining " + this.homeChannel)
    this.sendData(out, "JOIN " + this.homeChannel)
    //kill inactive querys
    val querys = this.conversations.keys.toVector.filter(!_.isChannel)
    if(!querys.isEmpty){
    println("Going into query removal")
    querys.foreach(print(_))
    val removedQuerys = Buffer[Conversation]()
    for(query <- querys) {
      if(query.recipient != sender && msg.ms - query.lastMessage.ms < 86400000) {
        this.conversations -= query
        removedQuerys += query
        query.kill
      }
      if(!removedQuerys.isEmpty) {
        println("Removed querys with " + removedQuerys.map(_.recipient).mkString(" ", ", ", "."))
      }
    }
  }
  }

  /**
   * Parts all channels. Auxilliary method for latebot.shutDown()
   *
   * @param out an output writer
   * @tparam out BufferedWriter
   */
  def shutDownBroadcast(out: BufferedWriter, prtMsg: String) = {
    this.conversations.keys.toVector.filter(_.isChannel).foreach((c: Conversation) => c.sendData(out, "PART " + c.recipient + " :" + prtMsg))
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
  def placeLine(msg, out: BufferedWriter) = {
    if (msg.address.lift(0).isDefined) {
      if (!this.conversations.find(_._1.recipient == msg.address).isDefined) {
        val newConversation = this.addConversation(msg.address, out)
        this.conversations(newConversation) += msg
        new Thread(newConversation).start()
      } else {
        val targetConversation = this.conversations.keys.find(_.recipient == msg.address).get
        this.conversations(targetConversation) += msg
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
  def pong(out: BufferedWriter, msg: Message) {
    if (msg.dataSplit.substring(0, 4).equalsIgnoreCase("ping")) {
      val pongmsg = "pong " + dataSplit.substring(5)
      sendData(out, pongmsg)
    }
  }
  /**
   * Finds the sender of the message.
   *
   * @params line the line from which the address is to be found
   * @returns the sender of the message
   */

  def address(line: String): String = {
    if(line.contains("irc.cs.hut.fi")){
    "Server"
    } else {
      var recipent = line.split("PRIVMSG ")(1).takeWhile(_ != ' ')
      if (recipent(0) == '#' || recipent(0) == '!') {
        recipent
      } else {
        recipent = line.split(":")(1).split("!")(0)
        recipent
      }
    }
  }
  /**
   * Sends a desired message one line at the time.
   *
   * @params out an output writer
   * @tparams out BufferedWriter
   * @params address the desired address for the message
   * @tparams address String
   * @params textToScroll the text which is to be sent one line at a time
   * @tparams textToScroll String
   */
  def scroller(out: BufferedWriter, address: String, textToScroll: String) = {
    textToScroll.split("\n").foreach(sendMessage(out, _, address))
  }

  /**
   * Relays the desired message "as is" to the server.
   *
   * @params out an output writer
   * @tparams out BufferedWriter
   * @params line the line containing the command word and the relayed message
   * @tparams line String
   */
  def relay(out: BufferedWriter, msg: Message) = {
    this.sendData(out, msg.raw.split("!relay ").lift(1).getOrElse(''))
  }
  /**
   * Main method for object for launching the bot.
   */
  def main(cmd: Array[String]) {
    connect("irc.cs.hut.fi", 6668)
  }

  /**
   * Obsolete testing method for spammers. Adds a Chatter to the spammer list.
   *
   * @params spammer the chatter to add to the list of known spammers
   * @tparams spammer Chatter
   */
  def addToBlackList(spammer: Chatter): Unit = {
    if (this.blackList.keys.find(_ == spammer).isDefined) {
      this.blackList(spammer) += 1
    } else {
      this.blackList += spammer -> 1
    }
  }

  /**
   * Obsolete testing method for spammers. Removes a Chatter from list of known spammers.
   *
   * @params nick the nick to be removed from the list of known spammers
   * @tparams nick String
   */

  def cleanReputation(nick: String) = {
    val toClean = this.blackList.keys.find(_.nick == nick)
    if (toClean.isDefined) {
      this.blackList -= toClean.get
      this.banList -= toClean.get
      println("Cleaned the reputation of " + nick)
    }
  }

  /**
   * Creates a new conversation, be it either Channel or Query, adds it to the
   * collection Conversations along with its incoming message Queue, and returns
   * the created conversation for launch.
   *
   * @params recipient the desired recipient for the conversation
   * @tparams recipient String
   * @params out an output writer
   * @tparams out BufferedWriter
   *
   * @returns the created Conversation
   */

  def addConversation(recipient: String, out: BufferedWriter) = {
    val newIncomingQueue = Queue[Message]()
    val newConversation = if (recipient(0) == '#') { new Channel(recipient, newIncomingQueue, out, this.homeChannel, this, 20, new Queue[Message]) } else { new Query(recipient, newIncomingQueue, out, this.homeChannel, this, 10, new Queue[Message]) }
    this.conversations += ((newConversation, newIncomingQueue))
    newConversation
  }
  /**
   * Adds a new query. Works as this.addConversation. No idea if this is obsolete or not.
   *
   * @params recipient the desired recipient for the conversation
   * @tparams recipient String
   * @params out an output writer
   * @tparams out BufferedWriter
   *
   * @returns the created Query
   */
  def addQuery(recipient: String, out: BufferedWriter) = {
    val newIncomingQueue = Queue[Message]()
    val newQuery = new Query(recipient, newIncomingQueue, out, this.homeChannel, this, 10, new Queue[Message])
    this.conversations += ((newQuery, newIncomingQueue))
    newQuery
  }

  /**
   * Joins a new channel, creates the associated Channel object and starts it
   * in a new Thread. Sends the hardcoded hello-message to the new channel.
   *
   *
   * @params line String containing the channel to be joined
   * @tparams line String
   * @params out an output writer
   * @tparams out BufferedWriter
   * @params input parameter to choose whether the method is called from the code or from user. if the parameter is "line" the input is from user, otherwise the method call is internal.
   * @tparams input String
   */

  def joinChannel(msg: Message, out: BufferedWriter, input: String) = {
    val channel = if (input == "line") { msg.raw.split("!join ")(1) } else { line }
    val newConversation = this.addConversation(channel, out)
    this.sendData(out, "JOIN " + channel)
    new Thread(newConversation).start()
    this.scroller(out, newConversation.recipient, this.hello)

  }
  /**
   * Unbans user. Obsolete.
   */
  def unBan(chatter: Chatter, channel: String, out: BufferedWriter) = {
    this.sendData(out, "MODE " + channel + " *" + chatter.hostmask + " -b")
    this.banList -= chatter
    println("Unbanned " + chatter.nick)
  }
  /**
   * Hair hat solution for converting time from milliseconds to understanble text.
   *
   * @params time System.currentTimeMillis()
   * @tparams time Long
   *
   * @returns the converted time in plaintext.
   */
  def convertTime(time: Long) = {
    val days = time / 86400000
    val hours = (time - days * 86400000) / 3600000
    val minutes = (time - days * 86400000 - hours * 3600000) / 60000
    val seconds = (time - days * 86400000 - hours * 3600000 - minutes * 60000) / 1000
    val millis = (time - days * 86400000 - hours * 3600000 - minutes * 60000 - seconds * 1000)
    days.toString + " days " + hours.toString() + " hours " + minutes.toString() + " minutes " + seconds.toString + "." + millis + " seconds"
  }

  /**
   * Prints the current status of the bot to the console.
   */

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

  /**
   * Toggles whether all incoming lines are printed to console or not.
   */

  def messagePrintToggle = {
    if (this.printMessagesToConsole) {
      println("No longer printing messages to console.")
      this.printMessagesToConsole = false
    } else {
      println("Messages will now be printed to console.")
      this.printMessagesToConsole = true
    }
  }

  /**
   * Celebrates birthday. Highly manual and works kinda wonky.
   *
   * @params out an output writer
   * @tparams out BufferedWriter
   * @params in an input reader
   * @tparams in BufferedReader
   * @params msg.raw the line containing the command word and a birthday boy.
   * @tparams msg.raw String
   */

  def birthday(out: BufferedWriter, in: BufferedReader, msg: Message) = {
    val birthdayBoy = msg.raw.split("!birthday ").lift(1).getOrElse("empty")
    if (birthdayBoy != "empty") {
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
  }

  /**
   * Parts a channel. Returns nothing.
   *
   * @params out an output writer
   * @tparams out BufferedWriter
   * @params msg.raw the line containing the command word and a birthday boy.
   * @tparams msg.raw String
   * @params nick the name of the channel
   * @tparams nick String
   */

  def part(out: BufferedWriter, msg: Message) = {
    val params = msg.raw.split("!part ").lift(1)
    val channel = params.getOrElse("empty").takeWhile(_ != ' ')
    var partMessage = params.getOrElse("empty").dropWhile(_ != ' ')
    if (partMessage.isEmpty) {
      partMessage = "You live another day, meatbags!"
    }
    if (this.conversations.keys.find(_.recipient == channel).isEmpty) {
      this.sendMessage(out, "No such channel.", msg.nick.get)
    } else {
      this.sendData(out, "PART " + channel + " :" + partMessage)
    }
  }

  /**
   * A simple file writer.
   */

  def writeToFile(fileName: String, toBeWritten: String) = this.synchronized {
    try {
      val writer = new FileWriter(fileName, true)
      writer.write(toBeWritten + System.getProperty("line.separator"))
      writer.close()
      println("Wrote [" + toBeWritten + "] to file " + fileName)
    } catch {
      case nofile: FileNotFoundException => println("No such file: " + fileName)
    }
  }

  /**
   * Sends irchelgmessage.txt to a recipient.
   */

  def helpNewUser(msg: Message, out: BufferedWriter) = {
    val channelName = msg.raw.split("JOIN ")(1).dropWhile(_ == ':')
    if (tutorialModeConversations.contains(channelName)) {
      val recipient = msg.raw.dropWhile(_ == ':').takeWhile(_ != '!')
      println("Tutoring user " + recipient + " at " + channelName + ".")
      conversations.keys.find(_.recipient == channelName).get.fileReader(out, recipient, "irchelpmessage.txt")
    }
  }
  /**
   * Puts channel in tutorial mode. When in tutorial mode, the channel automatically sends irchelpmessage
   * to every user joining the channel.
   */
  def addTutorialModeChannel(msg: Message, out: BufferedWriter) = {
    val channelToTutor = msg.raw.split("!addtutorial ").lift(1).getOrElse("empty")
    if (channelToTutor != "empty" && !this.tutorialModeConversations.contains(channelToTutor)) {
      this.tutorialModeConversations += channelToTutor
      this.sendMessage(out, channelToTutor + " has been added to tutored channels.", msg.address)
    } else if (channelToTutor == "empty") {
      this.sendMessage(out, "Syntax error", msg.address)
    } else {
      this.sendMessage(out, channelToTutor + " is already tutored.", msg.address)
    }
  }

  /**
   * Removes tutorial mode from a channel.
   */

  def removeTutorialModeChannel(msg: Message, out: BufferedWriter) = {
    val channelToRemove = msg.raw.split("!removetutorial ").lift(1).getOrElse("empty")
    if (channelToRemove != "empty" && this.tutorialModeConversations.contains(channelToRemove)) {
      this.tutorialModeConversations -= channelToRemove
      this.sendMessage(out, channelToRemove + " has been removed from tutored channels.", msg.address)
    } else if (channelToRemove == "empty") {
      this.sendMessage(out, "Syntax error", msg.address)
    } else {
      this.sendMessage(out, channelToRemove + " is not tutored.", msg.address)
    }
  }

}
