package acrt.photometrycluster

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.ClusterEvent._
import akka.cluster.MemberStatus
import akka.cluster.typed._

object Subscriber {
    sealed trait SubscriberCommand
    case class GetSubscription[A](requester:ActorRef[A]) extends SubscriberCommand
    def apply():Behavior[MemberEvent] = Behaviors.setup{context =>
        val cluster = Cluster(context.system)
        cluster.subscriptions ! Subscribe(context.self, classOf[MemberEvent])
        Behaviors.receiveMessage{message => 
            message match {
                case MemberUp(member) => {
                    if(member.hasRole("frontend")) {
                        
                    } else if(member.hasRole("backend")) {

                    } else {
                        context.log.warn("Member with unknown role in Subscriber.")
                    }
                }
            }
            Behaviors.same    
        }    
    }
}
