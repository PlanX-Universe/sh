package org.planx.sh.utility

import java.io.InputStream

object Resources {
  final val repositoryPath = getClass.getResource("/repository").getPath
  def getDomainPath(domain: String): String = getClass.getResource("/repository/" + domain + "/" + domain + ".hpdl").getPath
  def getDomain(domain: String): InputStream = getClass.getResourceAsStream("/repository/" + domain + "/" + domain)
  def getProblemPath(domain: String, problem: String): String = getClass.getResource("/repository/" + domain + "/" + problem + ".hpdl").getPath
}
