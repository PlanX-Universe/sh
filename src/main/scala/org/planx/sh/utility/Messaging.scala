package org.planx.sh.utility

import com.typesafe.config.{Config, ConfigFactory}

object Messaging {
  val config: Config = ConfigFactory.load()
  private val planning_system_name = config.getString("planning_system.name")
  private val planning_system_message_head = "[" + planning_system_name  + "] "
  def printPlanningSystemComponentMessage(component: String): String = "[" + planning_system_name + "." + component + "] "
  def printPlanningSystemMessage: String =  planning_system_message_head
}
