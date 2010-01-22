
#ifndef __StorProvOrb_INCLUDE_
#define __StorProvOrb_INCLUDE_

#include "OrbBase.hpp"
#include "SocketBase.hpp"
#include "StorProvSvrInterface.hpp"

namespace SynerEdge {

class StorProvOrb {

public:
	StorProvOrb(ClientSocket &sock, StorProvSvrInterface &spsi) ;
	~StorProvOrb() ;

	void processCalls() ;
	bool shutdown() ;

private:
	OrbBase *orb ;
	ServerInterface *si ;
	ClientSocket &socket ;

} ; // class StorProvOrb

} ; // namespace SynerEdge

#endif // __StorProvOrb_INCLUDE_
