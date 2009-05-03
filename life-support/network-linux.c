/* -*- Mode: C; Tab-Width: 4 -*- */

/* VLM Network Life Support for Linux */

#include <netdb.h>
#include <netinet/in.h>
#include "pfilt_wrapper.h"
#include <netinet/ip.h>
#include <arpa/inet.h>
#include <sys/ioctl.h>

#include "life_types.h"
#include "embed.h"
#include "VLM_configuration.h"
#include "life_prototypes.h"
#include "utilities.h"
#include "FEPComm.h"


/* Create the network channels */

void InitializeNetworkChannels (VLMConfig* config)
{
  struct ifconf ifc;
  int ipSocket, savedLen, i;
  bool tryAgain;

	ipSocket = socket (PF_INET, SOCK_STREAM, 0);
	if (ipSocket == -1)
		vpunt (NULL, "Unable to open IP socket to gather network interface information");

	ifc.ifc_len = 32 * sizeof (struct ifreq);
	ifc.ifc_buf = NULL;
	tryAgain = TRUE;

    while (tryAgain)
	  {
		ifc.ifc_buf = realloc (ifc.ifc_buf, ifc.ifc_len);
		if (ifc.ifc_buf == NULL)
			vpunt (NULL, "Unable to obtain space to read IP addresses of network interfaces");
		savedLen = ifc.ifc_len;
		if (ioctl (ipSocket, SIOCGIFCONF, &ifc) < 0)
			vpunt (NULL, "Unable to obtain IP addresses assigned to network interfaces");
		if (ifc.ifc_len == savedLen)
			ifc.ifc_len = 2 * ifc.ifc_len;
		else
		  tryAgain = FALSE;
	  }

	ifc.ifc_len = ifc.ifc_len / sizeof (struct ifreq);

	for (i = 0; i < MaxNetworkInterfaces; i++)
		if (config->interfaces[i].present)
			InitializeNetChannel (&config->interfaces[i], i, ipSocket, &ifc);

	close (ipSocket);

#ifdef MINIMA
	WriteFEPCommSlot (localIPAddress0, 0, Type_Fixnum);
	WriteFEPCommSlot (diagnosticIPAddress, htonl (config->diagnosticIPAddress.s_addr), Type_Fixnum);
	WriteFEPCommSlot (localIPAddress1, 0, Type_Fixnum);
	WriteFEPCommSlot (localIPSubnetMask0, 0, Type_Fixnum);
	WriteFEPCommSlot (localIPSubnetMask1, 0, Type_Fixnum);
	WriteFEPCommSlot (gatewayIPAddress0, 0, Type_Fixnum);
	WriteFEPCommSlot (gatewayIPAddress1, 0, Type_Fixnum);
	WriteFEPCommSlot (loadServerIPAddress, 0, Type_Fixnum);
#endif
}


/* Create a single network channel */

static void InitializeNetChannel (NetworkInterface* interface, int unitNumber, 
								  int ipSocket, struct ifconf* ifc)
{
  EmbPtr cp = EmbCommAreaAlloc (sizeof (EmbNetChannel));
  register EmbNetChannel* p = (EmbNetChannel*) HostPointer (cp);
  struct ifreq ifr;
  struct if_nameindex *saved_ifs, *ifs;
  /* A packet filter which will reject IP packets destined for the host */
  struct sock_filter localFilters[N_FILTERS] =
	{BPF_STMT(BPF_LD+BPF_H+BPF_ABS, 0),
	 BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, ETHERTYPE_IP, 0, 3),
	 BPF_STMT(BPF_LD+BPF_W+BPF_ABS, 0),
	 BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, 0, 0, 1),
	 BPF_STMT(BPF_RET+BPF_K, 0),
	 BPF_STMT(BPF_RET+BPF_K, (u_int)-1),
    };
  u_short etherTypeOffset = offsetof (struct ether_header, ether_type) / sizeof (u_short);
  u_short ipAddressOffset
	= (offsetof (struct ip, ip_dst) + sizeof (struct ether_header)) / sizeof (u_short);
  int interfaceIndex, i;
  NetworkInterface* pInterface;
#ifdef GENERA
  struct in_addr guestAddress;
  char addressAsString[_POSIX_ARG_MAX];
  boolean firstInterface;
#endif

	p->type = EmbNetworkChannelType;
	p->unit = unitNumber;
	p->fd = -1;									/* Needed before linking into channel list */
	p->receiverThreadSetup = FALSE;				/* .. */
	p->next = EmbCommAreaPtr->channel_table;	/* Link into the channel list */
	EmbCommAreaPtr->channel_table = cp;

	if (interface->device[0])
	  {
		/* Verify that the requested device is running and is an Ethernet interface */

		p->name0 = p->name1 = 0;
		memcpy ((char*)&p->name0, interface->device, 2 * sizeof (EmbWord));

		strncpy (ifr.ifr_name, interface->device, IFNAMSIZ);

		if (ioctl (ipSocket, SIOCGIFINDEX, &ifr) < 0)
			vpunt (NULL, "Unable to determine interface index of network device %s",
				   interface->device);
		interfaceIndex = ifr.ifr_ifindex;

		if (ioctl (ipSocket, SIOCGIFFLAGS, &ifr) < 0)
			vpunt (NULL, "Unable to determine attributes of network device %s",
				   interface->device);
		if (ifr.ifr_flags & IFF_LOOPBACK)
			vpunt (NULL, "Unable to attach VLM network interface #%d to device %s"
						 " as it is a loopback device",
				   unitNumber, interface->device);
		if ((ifr.ifr_flags & (IFF_UP | IFF_RUNNING)) != (IFF_UP | IFF_RUNNING))
			vpunt (NULL, "Unable to attach VLM network interface #%d to device %s"
						 " as it is not up and running",
				   unitNumber, interface->device);

		if (ioctl (ipSocket, SIOCGIFHWADDR, &ifr) < 0)
			vpunt (NULL,
				   "Unable to determine hardware interface address for network device %s",
				   interface->device);
		if (ifr.ifr_hwaddr.sa_family != ARPHRD_ETHER)
			vpunt (NULL, "Unable to attach VLM network interface #%d to device %s"
						 " as it does not use Ethernet packet formats",
				   unitNumber, interface->device);
		p->hardwareAddressHigh = p->hardwareAddressLow = 0;
		memcpy ((char*)&p->hardwareAddressHigh, ifr.ifr_hwaddr.sa_data, 2 * sizeof (EmbWord));
	  }

	else
	  {
		/* No interface specified: Use the first available Ethernet interface */
		interfaceIndex = -1;
		ifs = saved_ifs = if_nameindex();

		while (ifs->if_index != 0 && ifs->if_name != NULL)
		  {
			strncpy(ifr.ifr_name, ifs->if_name, IFNAMSIZ);
			if (ioctl (ipSocket, SIOCGIFFLAGS, &ifr) < 0)
				vpunt (NULL, "Unable to determine attributes of network device %s",
					   ifr.ifr_name);
			if ((ifr.ifr_flags & (IFF_UP | IFF_RUNNING | IFF_LOOPBACK))
				== (IFF_UP | IFF_RUNNING))
			  {
				if (ioctl (ipSocket, SIOCGIFHWADDR, &ifr) < 0)
					vpunt (NULL, "Unable to determine hardware address for network device %s",
						   ifr.ifr_name);
				if (ifr.ifr_hwaddr.sa_family == ARPHRD_ETHER)
				  {
					interfaceIndex = ifs->if_index;
					strncpy (interface->device, ifs->if_name, IFNAMSIZ);
					p->name0 = p->name1 = 0;
					memcpy ((char*)&p->name0, ifs->if_name, 2 * sizeof (EmbWord));
					p->hardwareAddressHigh = p->hardwareAddressLow = 0;
					memcpy ((char*)&p->hardwareAddressHigh, ifr.ifr_hwaddr.sa_data,
							2 * sizeof (EmbWord));
					break;
					
				  }
			  }
			ifs++;
		  }

		if_freenameindex (saved_ifs);

		if (interfaceIndex < 0)
			vpunt (NULL, "Unable to find an Ethernet interface to attach"
						 " to VLM network interface #%d",
				   unitNumber);
	  }


	/* Get IP address of interface */

	p->hostPrimaryProtocol = -1;

	for (i = 0; i < ifc->ifc_len; i++) 
	  {
		if (strncmp (interface->device, ifc->ifc_req[i].ifr_name, IFNAMSIZ) == 0)
		  {
			p->hostPrimaryProtocol = ETHERTYPE_IP;
			p->hostPrimaryAddress
			  = ((struct sockaddr_in *)&ifc->ifc_req[i].ifr_addr)->sin_addr.s_addr;
			break;
		  }
	  }

	if (p->hostPrimaryProtocol == -1)
		vpunt (NULL, "Unable to determine IP address assigned to network device %s",
			   interface->device);


	/* Open a packet socket and bind it to the interface */

#ifndef NOROOT
	p->fd = socket (PF_PACKET, SOCK_RAW, htons (ETH_P_ALL));
	if (p->fd < 0)
		vpunt (NULL, "Unable to open packet socket for VLM network interface #%d",
			   unitNumber);
#endif

	memset (&p->sll, 0, sizeof (p->sll));
	p->sll.sll_family   = AF_PACKET;
	p->sll.sll_ifindex  = interfaceIndex;
	p->sll.sll_protocol = htons (ETH_P_ALL);

#ifndef NOROOT
	if (bind (p->fd, (struct sockaddr *)&p->sll, sizeof (p->sll)) < 0)
		vpunt (NULL, "Unable to attach VLM network interface #%d to device %s",
			   unitNumber, interface->device);
#endif

	p->sll.sll_protocol = 0;			/* Transmission requires this value be zero... */
	p->sll.sll_halen = ETH_ALEN;


	/* Set attributes (e.g., copyall, not promiscuous */

#ifdef OS_OSF
	ioctlBits = ENHOLDSIG | ENNONEXCL | ENCOPYALL;
	if (-1 == ioctl (p->fd, EIOCMBIS, &ioctlBits))
		vpunt (NULL, "Unable to set attributes for VLM network interface #%d", unitNumber);

	ioctlBits = ENBATCH | ENTSTAMP | ENPROMISC | ENBPFHDR;
	if (-1 == ioctl (p->fd, EIOCMBIC, &ioctlBits))
		vpunt (NULL, "Unable to clear attributes for VLM network interface #%d", unitNumber);

	timeout.tv_sec = timeout.tv_usec = 0;		/* Wait indefinitely for packets */
	if (-1 == ioctl (p->fd, EIOCSRTIMEOUT, &timeout))
		vpunt (NULL, "Unable to set packet timeout for VLM network interface #%d", unitNumber);

	x = deviceParms.end_MTU;					/* TEMPORARY workaround to DEC bug */
	x = (x < MaxEmbNetPacketSize) ? x : MaxEmbNetPacketSize;
	if (-1 == ioctl (p->fd, EIOCTRUNCATE, &x))
		vpunt (NULL, "Unable to set maximum packet size for VLM network interface #%d",
			   unitNumber);

	x = -1;										/* -1 => Get maximum allowable queue size */
	if (-1 == ioctl (p->fd, EIOCMAXBACKLOG, &x))
		vpunt (NULL, "Unable to determine maximum queue size for VLM network interface #%d", 
			   unitNumber);
	if (-1 == ioctl (p->fd, EIOCSETW, &x))
		vpunt (NULL, "Unable to set queue size for VLM network interface #%d", unitNumber);
#endif


	/* Create and attach the filter program */
	
	localFilters[0].k = etherTypeOffset;
	localFilters[2].k = ipAddressOffset;
	localFilters[3].k = p->hostPrimaryAddress;

	memcpy (p->filter.filters, localFilters, sizeof (localFilters));
	p->filter.fprog.len = N_FILTERS;
	p->filter.fprog.filter = (struct sock_filter*)&p->filter.filters;

#ifndef NOROOT
	if (setsockopt (p->fd, SOL_SOCKET, SO_ATTACH_FILTER,
					&p->filter.fprog, sizeof (struct sock_fprog)))
		vpunt (NULL, "Unable to set packet filter program for VLM network interface #%d",
			   unitNumber);
#endif


	/* Create entries in the host's ARP table for each IP address assigned to this channel */

	p->arpReq = NULL;

#ifdef GENERA
	for (pInterface = interface; pInterface != NULL; pInterface = pInterface->anotherAddress)
#else
	pInterface = interface;
#endif
	  {
		if (pInterface->myProtocol == ETHERTYPE_IP)
		  {
			EmbPtr arpReqPtr = EmbCommAreaAlloc (sizeof (EmbNetARPReq));
			register EmbNetARPReq* pARP = (EmbNetARPReq*) HostPointer (arpReqPtr);
			pARP->next = p->arpReq;
			p->arpReq = pARP;

			pARP->arp.arp_pa.sa_family = AF_INET;
			((struct sockaddr_in *)&pARP->arp.arp_pa)->sin_addr.s_addr
			  = htonl (pInterface->myAddress.s_addr);

			pARP->arp.arp_ha.sa_family = ARPHRD_ETHER;	/* Only supported interface type */
			memcpy (pARP->arp.arp_ha.sa_data, &p->hardwareAddressHigh, 2 * sizeof (EmbWord));

			pARP->arp.arp_flags = ATF_COM | ATF_PERM /* | ATF_PUBL */ ;
			memcpy (pARP->arp.arp_dev, interface->device, sizeof (pARP->arp.arp_dev));
										/* Only first interface structure has the device */

#ifndef NOROOT
			if (ioctl (ipSocket, SIOCSARP, &pARP->arp) < 0)
				vpunt (NULL, "Unable to establish ARP mappings for VLM network interface #%d",
					   unitNumber);
#endif
		  }
	  }

	
	/* Finish initialization */

	p->status = 0;
	p->guestPrimaryProtocol = interface->myProtocol;
	p->guestPrimaryAddress = htonl (interface->myAddress.s_addr);

#if BYTE_ORDER == BIG_ENDIAN
	bswap32_block (&p->hardwareAddressHigh, 2 * sizeof (EmbWord));
#endif

	p->nTransmitFailures = p->nReceiveFailures = 0;

	p->guestToHostQueue = CreateQueue (NetworkTransmitterQueueSize, sizeof (EmbPtr));
	p->guestToHostQ = (EmbQueue*) HostPointer (p->guestToHostQueue);
	p->guestToHostQ->signal = InstallSignalHandler ((ProcPtrV) &NetworkChannelTransmitter,
												    (PtrV) p, FALSE);

	p->guestToHostReturnQueue = CreateQueue (NetworkTransmitterQueueSize, sizeof (EmbPtr));
	p->guestToHostReturnQ = (EmbQueue*) HostPointer (p->guestToHostReturnQueue);

	p->hostToGuestSupplyQueue = CreateQueue (NetworkReceiverQueueSize, sizeof (EmbPtr));
	p->hostToGuestSupplyQ = (EmbQueue*) HostPointer (p->hostToGuestSupplyQueue);

	p->hostToGuestQueue = CreateQueue (NetworkReceiverQueueSize, sizeof (EmbPtr));
	p->hostToGuestQ = (EmbQueue*) HostPointer (p->hostToGuestQueue);

#ifdef GENERA
	for (pInterface = interface, firstInterface = TRUE; pInterface != NULL;
		 pInterface = pInterface->anotherAddress, firstInterface = FALSE)
	  {
		if (firstInterface)
			addressAsString[0] = 0;
		else
			sprintf (addressAsString, "%s,", addressAsString);
		if (pInterface->device[0])
			sprintf (addressAsString, "%s%s:", addressAsString, pInterface->device);
		switch (pInterface->myProtocol)
		{
		  case ETHERTYPE_IP:
			guestAddress.s_addr = htonl (pInterface->myAddress.s_addr);
			sprintf (addressAsString, "%sINTERNET|%s", addressAsString, 
					 inet_ntoa (guestAddress));
			break;
		  case ETHERTYPE_CHAOS:
			sprintf (addressAsString, "%sCHAOS|%o", addressAsString,
					 htonl (pInterface->myAddress.s_addr));
			break;
		}
		if (pInterface->myOptions[0])
			sprintf (addressAsString, "%s;%s", addressAsString, pInterface->myOptions);
	  }
	p->addressString = MakeEmbString (addressAsString);
#endif

	if (pthread_create (&p->receiverThread, &EmbCommAreaPtr->inputThreadAttrs,
					    (pthread_startroutine_t) &NetworkChannelReceiver, (pthread_addr_t) p))
		vpunt (NULL,
			   "Unable to create thread to receive packets for VLM network interface #%d",
			   unitNumber);
	p->receiverThreadSetup = TRUE;

	p->status |= EmbNetStatusHostReady;
}


/* Reset a network channel */

void ResetNetworkChannel (EmbChannel* channel)
{
  register EmbNetChannel* netChannel = (EmbNetChannel*) channel;

#ifdef OS_OSF
	ioctl (netChannel->fd, EIOCFLUSH, 0); 	/* Flush incoming packets */
#endif

	ResetIncomingQueue (netChannel->guestToHostQ);
	ResetOutgoingQueue (netChannel->guestToHostReturnQ);

	ResetIncomingQueue (netChannel->hostToGuestSupplyQ);
	ResetOutgoingQueue (netChannel->hostToGuestQ);
}


/* Network Channel transmitter */

static void NetworkChannelTransmitter (EmbNetChannel* pNetChannel)
{
  register EmbNetChannel* netChannel = pNetChannel;
  register EmbQueue* transmitQueue = netChannel->guestToHostQ;
  register EmbQueue* returnQueue = netChannel->guestToHostReturnQ;
  EmbPtr netPacketPtr;
  EmbNetPacket* netPacket;
  ssize_t nBytes, actualBytes;

	while (EmbQueueFilled (transmitQueue))
	  {
		if (0 == EmbQueueSpace (returnQueue))
		  {
			/* Can't do I/O now because we can't return the buffer -- Try again later */
			SignalLater (transmitQueue->signal);
			return;
		  }

		netPacketPtr = EmbQueueTakeWord (transmitQueue);
		if (NULL == (void*)(uint64_t)netPacketPtr) netPacketPtr = NullEmbPtr;

		if (netPacketPtr != NullEmbPtr)
		  {
			if (netChannel->status & EmbNetStatusHostReady)
			  {
				netPacket = (EmbNetPacket*) HostPointer (netPacketPtr);
				nBytes = (ssize_t) netPacket->nBytes;
#if BYTE_ORDER == BIG_ENDIAN
				bswap32_block (&netPacket->data, nBytes);
#endif
				memcpy (netChannel->sll.sll_addr, ((struct ethhdr*)netPacket->data)->h_dest,
						ETH_ALEN);
				actualBytes = sendto (netChannel->fd, &netPacket->data[0],
									  nBytes, MSG_CONFIRM,
									  (struct sockaddr*)&netChannel->sll,
									  sizeof (netChannel->sll));
				if (actualBytes != nBytes)
				  {
					netChannel->nTransmitFailures++;
				  }
			  }

			EmbQueuePutWord (returnQueue, netPacketPtr);
		  }
	  }
}


/* Network Channel receiver thread -- Can it be written to not copy??? */

#define OneMillisecond 1000000L

static void NetworkChannelReceiver (pthread_addr_t argument)
{
  pthread_t self = pthread_self ();
  register EmbNetChannel* netChannel = (EmbNetChannel*) argument;
  register EmbQueue* supplyQueue = netChannel->hostToGuestSupplyQ;
  register EmbQueue* receiveQueue = netChannel->hostToGuestQ;
  struct pollfd pollReceiver;
  struct timespec receiverPause;
  struct sockaddr_ll sll;
  EmbPtr netPacketPtr;
  EmbNetPacket* netPacket;
  ssize_t actualBytes;
  socklen_t sllLen;

	pthread_cleanup_push ((pthread_cleanuproutine_t)pthread_detach, (void*)self);

	WaitUntilInitializationComplete ();

	pollReceiver.fd = netChannel->fd;
	pollReceiver.events = POLLIN;

	while (TRUE)
	  {
		pthread_testcancel ();
		
		pollReceiver.revents = 0;
		poll (&pollReceiver, 1, 1000);

		if (0 == (pollReceiver.revents & POLLIN))
			continue;

		sllLen = sizeof (sll);
		actualBytes = recvfrom (netChannel->fd, &netChannel->receiveBuffer,
								MaxEmbNetPacketSize, MSG_TRUNC,
								(struct sockaddr*)&sll, &sllLen);

		if (actualBytes > MaxEmbNetPacketSize)
			actualBytes = MaxEmbNetPacketSize;

		if (actualBytes < 0)
			netChannel->nReceiveFailures++;

		else if (0 == actualBytes)
			netChannel->nFalseReceiverWakeups++;

		else if (!(netChannel->status & EmbNetStatusGuestReady))
			;

		else if ((0 == EmbQueueSpace (supplyQueue)) || (0 == EmbQueueSpace (receiveQueue)))
			netChannel->nReceivedPacketsLost++;

		else
		  {
			while (0 == (netPacketPtr = EmbQueueTakeWord (supplyQueue)))
			  {
				receiverPause.tv_sec = 0;
				receiverPause.tv_nsec = OneMillisecond;
				if (pthread_delay_np (&receiverPause))
					vpunt (NULL, "Unable to sleep in thread %lx", self);
			  }
			netPacket = (EmbNetPacket*) HostPointer (netPacketPtr);
			netPacket->nBytes = (EmbWord) actualBytes;
			memcpy (&netPacket->data[0], &netChannel->receiveBuffer[0], actualBytes);
#if BYTE_ORDER == BIG_ENDIAN
			bswap32_block (&netPacket->data, actualBytes);
#endif
			EmbQueuePutWord (receiveQueue, netPacketPtr);
		  }
	  }

	pthread_cleanup_pop (TRUE);
}


/* Cleanup a single network channel */

static void TerminateNetChannel (EmbNetChannel* netChannel, int ipSocket)
{
  EmbNetARPReq *embARPReq;
  void *exit_value;

	if (netChannel->receiverThreadSetup)
	  {
		pthread_cancel (netChannel->receiverThread);
		pthread_join (netChannel->receiverThread, &exit_value);
		netChannel->receiverThreadSetup = FALSE;
	  }

#ifndef NOROOT
	for (embARPReq = netChannel->arpReq;  embARPReq != NULL; embARPReq->next)
		ioctl (ipSocket, SIOCDARP, &embARPReq->arp);
#endif

	if (netChannel->fd != -1)
	  {
		close (netChannel->fd);
		netChannel->fd = -1;
	  }
}


/* Cleanup the network channels */

void TerminateNetworkChannels ()
{
  EmbNetChannel* netChannel;
  EmbPtr channel;
  int ipSocket;

	ipSocket = socket (PF_INET, SOCK_STREAM, 0);

	for (channel = EmbCommAreaPtr->channel_table; channel != NullEmbPtr;
		 channel = netChannel->next)
	  {
		netChannel = (EmbNetChannel*) HostPointer (channel);
		if (EmbNetworkChannelType == netChannel->type)
			TerminateNetChannel (netChannel, ipSocket);
	  }

	if (ipSocket > -1)
		close (ipSocket);
}
