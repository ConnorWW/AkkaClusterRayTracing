package acrt.cluster

import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.typed.Cluster
import com.typesafe.config.ConfigFactory

object App {

  object RootBehavior {
    def apply(): Behavior[Nothing] = Behaviors.setup[Nothing] { ctx =>
      val cluster = Cluster(ctx.system)

      if (cluster.selfMember.hasRole("backend")) {
        val workersPerNode =
          ctx.system.settings.config.getInt("transformation.workers-per-node")
        (1 to workersPerNode).foreach { n =>
          ctx.spawn(Worker(), s"Worker$n")
        }
      }
      if (cluster.selfMember.hasRole("frontend")) {
        ctx.spawn(Frontend(), "Frontend")
      }
      Behaviors.empty
    }
  }

  def main(args: Array[String]): Unit = {
    // starting 2 frontend nodes and 3 backend nodes
    if (args.isEmpty) {
      startup("backend", 25251, "131.194.71.132")
      startup("backend", 25252, "131.194.71.132")
      startup("frontend", 0, "131.194.71.132")
      startup("frontend", 0, "131.194.71.132")
      startup("frontend", 0, "131.194.71.132")
    } else {
      require(args.length == 3, "Usage: role port ip")
      startup(args(0), args(1).toInt, args(2))
    }
  }

  def startup(role: String, port: Int, ip: String): Unit = {
    // Override the configuration of the port and role
    val config = ConfigFactory
      .parseString(s"""
        akka.remote.artery.canonical.hostname = "$ip"
        akka.remote.artery.canonical.port=$port
        akka.cluster.roles = [$role]
        """)
      .withFallback(ConfigFactory.load("transformation"))

    ActorSystem[Nothing](RootBehavior(), "ClusterSystem", config)

  }

}
