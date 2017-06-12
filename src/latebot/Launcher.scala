package latebot

object Launcher extends App{
  
  val l = new latebot
  l.connect("irc.cs.hut.fi", 6668)
  
}