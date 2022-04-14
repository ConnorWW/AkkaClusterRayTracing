package acrt.photometrycluster

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors


object FrontendNode {
    //controls the ImageDrawer and GeometryCreator
    sealed trait FrontendNodeCommand
    def apply():Behavior[FrontendNodeCommand] = {
        Behaviors.setup{ context => 
            Behaviors.receiveMessage{ message => message match {
                case m => context.log.info(s"Unhandled message $m received in FrontendNode")
            }  
            Behaviors.same    
            }
        }
    }
}
