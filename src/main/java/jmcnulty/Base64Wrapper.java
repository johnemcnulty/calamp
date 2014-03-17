package jmcnulty;

import com.google.common.base.Charsets;
import com.google.common.io.BaseEncoding;

public class Base64Wrapper {
	public static byte[] decode(String s) {
		return BaseEncoding.base64().decode(s);
	}
	public static String encode(String s) {
		return BaseEncoding.base64().encode(s.getBytes(Charsets.UTF_8));
	}
}
