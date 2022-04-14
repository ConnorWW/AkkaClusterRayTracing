package acrt.photometrycluster

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.ClusterEvent._
import akka.cluster.MemberStatus
import akka.cluster.typed._

object BackendNode {
    sealed trait BackendNodeCommand
    case class WrappedMemberEvent(m: MemberEvent) extends BackendNodeCommand
    
    //contains one geometryManagers per file assigned as well as child intersectors
    def apply():Behavior[BackendNodeCommand] = {
        Behaviors.setup{ context => 
            val memberEventWrapper:ActorRef[MemberEvent] = context.messageAdapter(mem => WrappedMemberEvent(mem))
            val cluster = Cluster(context.system)
            cluster.subscriptions ! Subscribe(memberEventWrapper, classOf[MemberEvent])
            Behaviors.receiveMessage{ message => message match {
                case WrappedMemberEvent(memberEvent) => { memberEvent match {
                    case MemberUp(member) => {
                        ???
                    }
                    case m => 
                }
                    
                }
                case m => context.log.info(s"Unhandled message $m received in BackendNode")
            }  
            Behaviors.same    
            }
        }
    }
}
