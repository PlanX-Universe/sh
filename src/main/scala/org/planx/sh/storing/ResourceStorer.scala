package org.planx.sh.storing

import org.planx.sh.utility.Resources
import java.io.{File, FileWriter}

object ResourceStorer {

  def storeDomainFileInRepository(domainName: String, file: String): Boolean = {
      if(!repositoryExists(domainName)) {
        new File(Resources.repositoryPath + "/" + domainName).mkdirs()
        writeHPDLFile(domainName, domainName, file)
        true
      } else {
        false
      }
  }

  def storeProblemFileInRepository(domainName:String, problemName:String, file: String): Boolean = {
    if (repositoryExists(domainName)) {
      if(domainName.equals(problemName)) return false
      writeHPDLFile(domainName, problemName, file)
      true
    } else {
      false
    }
  }

  private def repositoryExists(domainName: String): Boolean = new File(Resources.repositoryPath + "/" + domainName).exists
  private def writeHPDLFile(folderName: String, fileName: String, file: String): Unit = {
    val fileWriter = new FileWriter(new File(Resources.repositoryPath + "/" + folderName + "/" + fileName + ".hpdl"))
    fileWriter.write(file)
    fileWriter.close()
  }
}