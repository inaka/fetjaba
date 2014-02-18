package net.inaka.fetjaba;

import java.io.IOException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

/**
 * @author Fernando Benavides <elbrujohalcon@inaka.net> Entry point for this
 *         application
 */
public class Node {
	/** This Erlang node */
	public static OtpNode NODE;

	/** Peer node name */
	public static String PEER;

	/**
	 * @param args
	 *            command line arguments
	 */
	public static void main(String[] args) {
		String peerName = args.length >= 1 ? args[0] : "fetjaba@nohost";
		String nodeName = args.length >= 2 ? args[1] : "fetjaba_java@nohost";
		try {
			NODE = args.length >= 3 ? new OtpNode(nodeName, args[2])
					: new OtpNode(nodeName);
			PEER = peerName;

			// We create an mbox to link
			final OtpMbox mbox = NODE.createMbox("fetjaba_server");
			new Thread(mbox.getName()) {
				@Override
				public void run() {
					boolean run = true;
					while (run) { // This thread runs forever
						try {
							OtpErlangObject msg = mbox.receive();
							run = processMsg(msg, mbox);
						} catch (OtpErlangExit oee) {
							System.exit(1);
						} catch (OtpErlangDecodeException oede) {
							oede.printStackTrace();
							System.out.println("bad message, moving on...");
						}
					}
				}
			}.start();
			System.out.println("READY");
		} catch (IOException e1) {
			e1.printStackTrace();
			System.exit(1);
		}
	}

	protected static boolean processMsg(OtpErlangObject msg, OtpMbox mbox)
			throws OtpErlangDecodeException {
		OtpErlangLong ts2 = new OtpErlangLong(System.currentTimeMillis());
		if (msg instanceof OtpErlangAtom
				&& ((OtpErlangAtom) msg).atomValue().equals("stop")) {
			mbox.close();
			return false;
		} else if (msg instanceof OtpErlangTuple) {
			OtpErlangTuple cmd = (OtpErlangTuple) msg;
			OtpErlangAtom cmdName = (OtpErlangAtom) cmd.elementAt(0);
			OtpErlangPid caller = (OtpErlangPid) cmd.elementAt(1);
			OtpErlangObject result = new OtpErlangAtom("undefined");
			if (cmdName.atomValue().equals("pid")) {
				result = new OtpErlangTuple(new OtpErlangObject[] { cmdName,
						mbox.self() });
			} else if (cmdName.atomValue().equals("tick")) {
				result = new OtpErlangTuple(new OtpErlangObject[] { cmdName,
						mbox.self(), cmd.elementAt(2), cmd.elementAt(3), ts2,
						new OtpErlangLong(System.currentTimeMillis()) });
			}
			mbox.send(caller, result);
			return true;
		}
		throw new OtpErlangDecodeException("Bad message: " + msg.toString());
	}
}
