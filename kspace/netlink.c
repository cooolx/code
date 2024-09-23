#ifdef __EXP_NETLINK__

#include <linux/socket.h>
#include <linux/netlink.h>
#include <net/sock.h>


struct sock *nl_sk = NULL;
void nl_data_ready (struct sock *sk, int len)
{
	wake_up_interruptible(sk->sk_sleep);
}


void netlink_test(void)
{
	struct sk_buff *skb = NULL;
	struct nlmsghdr *nlh = NULL;
	int err;
	u32 pid;    

	nl_sk = netlink_kernel_create(&init_net, 0, 0, nl_data_ready, NULL, THIS_MODULE);
	printk("nl_sk=%p\n", nl_sk);
	skb = skb_recv_datagram(nl_sk, 0, 0, &err);
	nlh = (struct nlmsghdr *)skb->data;
	printk("%s: received netlink message payload:%s ",  __FUNCTION__, NLMSG_DATA(nlh));
#if 0
	pid = nlh->nlmsg_pid;
	NETLINK_CB(skb).groups = 0;
	NETLINK_CB(skb).pid = 0;     
	NETLINK_CB(skb).dst_pid = pid;
	NETLINK_CB(skb).dst_groups = 0; 
	netlink_unicast(nl_sk, skb, pid, MSG_DONTWAIT);
	sock_release(nl_sk->socket);
#endif
}




#endif
