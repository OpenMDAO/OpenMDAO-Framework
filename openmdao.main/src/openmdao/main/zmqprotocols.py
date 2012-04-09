
import zmq

PROTOCOLS = {
    'OMDAOCLI01': {
    },
    'OMDAOSRV01': {
    }
}


def process_msg(msg):
    """Takes a general multi-frame message of the form
    
       [protocol_id, msgtype_id, ...]
       
       and passes it to the appropriate handler based on protocol
       version and message type.
    """
    

