package org.jetbrains.sbt.resolvers.indexes

import com.intellij.openapi.progress.ProgressIndicator
import org.jetbrains.idea.maven.indices.{MavenIndex, MavenProjectIndicesManager}
import org.jetbrains.plugins.scala.project.ProjectContext
import org.jetbrains.sbt.resolvers.ArtifactInfo

import scala.collection.JavaConverters._

/**
  * @author Mikhail Mutcianko
  * @since 26.07.16
  */
class MavenProxyIndex(val root: String, val name: String) extends ResolverIndex {

  private val MAX_RESULTS = 1000

  override def doUpdate(progressIndicator: Option[ProgressIndicator] = None)(implicit project: ProjectContext): Unit = {
    findPlatformMavenResolver
      .foreach(i =>
        MavenProjectIndicesManager.getInstance(project)
          .scheduleUpdate(List(i).asJava)
      )
  }

  override def getUpdateTimeStamp(implicit project: ProjectContext): Long = {
    findPlatformMavenResolver.map(_.getUpdateTimestamp).getOrElse(ResolverIndex.NO_TIMESTAMP)
  }

  override def close(): Unit = ()

  override def searchGroup(artifactId: String)(implicit project: ProjectContext): Set[String] = {
    findPlatformMavenResolver.map { r =>
      if (artifactId != "")
        r.getGroupIds.asScala.filter(r.hasArtifactId(_, artifactId)).toSet
      else
        r.getGroupIds.asScala.toSet
    }.getOrElse(Set.empty)
  }

  override def searchArtifact(groupId: String)(implicit project: ProjectContext): Set[String] = {
    findPlatformMavenResolver.map {
      _.getArtifactIds(groupId).asScala.toSet
    }.getOrElse(Set.empty)
  }

  override def searchVersion(groupId: String, artifactId: String)(implicit project: ProjectContext): Set[String] = {
    findPlatformMavenResolver.map {
      _.getVersions(groupId, artifactId).asScala.toSet
    }.getOrElse(Set.empty)
  }

  private def findPlatformMavenResolver(implicit project: ProjectContext): Option[MavenIndex] = {
    MavenProjectIndicesManager.getInstance(project)
      .getIndices
      .asScala
      .find(_.getRepositoryPathOrUrl == MavenIndex.normalizePathOrUrl(root))
  }

  override def searchArtifactInfo(fqName: String): Set[ArtifactInfo] = Set.empty
}

