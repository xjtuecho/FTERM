{*
   FtermSSH : An SSH implementation in Delphi for FTerm2 by kxn@cic.tsinghua.edu.cn

   Cryptograpical code from OpenSSL Project

*}

unit sshconst;

interface

const
  SSH_CIPHER_SSH2 = -3;
  SSH_CIPHER_ILLEGAL = -2;
  SSH_CIPHER_NOT_SET = -1;
  SSH_CIPHER_NONE = 0;
  SSH_CIPHER_IDEA = 1;
  SSH_CIPHER_DES = 2;
  SSH_CIPHER_3DES = 3;
  SSH_CIPHER_BROKEN_TSS = 4;
  SSH_CIPHER_BROKEN_RC4 = 5;
  SSH_CIPHER_BLOWFISH = 6;
  SSH_CIPHER_RESERVED = 7;
  SSH_CIPHER_MAX = 31;

  SSH1_MSG_DISCONNECT = 1;
  SSH1_SMSG_PUBLIC_KEY = 2;
  SSH1_CMSG_SESSION_KEY = 3;
  SSH1_CMSG_USER = 4;
  SSH1_CMSG_AUTH_RSA = 6;
  SSH1_SMSG_AUTH_RSA_CHALLENGE = 7;
  SSH1_CMSG_AUTH_RSA_RESPONSE = 8;
  SSH1_CMSG_AUTH_PASSWORD = 9;
  SSH1_CMSG_REQUEST_PTY = 10;
  SSH1_CMSG_WINDOW_SIZE = 11;
  SSH1_CMSG_EXEC_SHELL = 12;
  SSH1_CMSG_EXEC_CMD = 13;
  SSH1_SMSG_SUCCESS = 14;
  SSH1_SMSG_FAILURE = 15;
  SSH1_CMSG_STDIN_DATA = 16;
  SSH1_SMSG_STDOUT_DATA = 17;
  SSH1_SMSG_STDERR_DATA = 18;
  SSH1_CMSG_EOF = 19;
  SSH1_SMSG_EXIT_STATUS = 20;
  SSH1_MSG_CHANNEL_OPEN_CONFIRMATION = 21;
  SSH1_MSG_CHANNEL_OPEN_FAILURE = 22;
  SSH1_MSG_CHANNEL_DATA = 23;
  SSH1_MSG_CHANNEL_CLOSE = 24;
  SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION = 25;
  SSH1_SMSG_X11_OPEN = 27;
  SSH1_CMSG_PORT_FORWARD_REQUEST = 28;
  SSH1_MSG_PORT_OPEN = 29;
  SSH1_CMSG_AGENT_REQUEST_FORWARDING = 30;
  SSH1_SMSG_AGENT_OPEN = 31;
  SSH1_MSG_IGNORE = 32;
  SSH1_CMSG_EXIT_CONFIRMATION = 33;
  SSH1_CMSG_X11_REQUEST_FORWARDING = 34;
  SSH1_CMSG_AUTH_RHOSTS_RSA = 35;
  SSH1_MSG_DEBUG = 36;
  SSH1_CMSG_REQUEST_COMPRESSION = 37;
  SSH1_CMSG_AUTH_TIS = 39;
  SSH1_SMSG_AUTH_TIS_CHALLENGE = 40;
  SSH1_CMSG_AUTH_TIS_RESPONSE = 41;
  SSH1_CMSG_AUTH_CCARD = 70;
  SSH1_SMSG_AUTH_CCARD_CHALLENGE = 71;
  SSH1_CMSG_AUTH_CCARD_RESPONSE = 72;
  SSH1_AUTH_TIS = 5;
  SSH1_AUTH_CCARD = 16;
  SSH1_PROTOFLAG_SCREEN_NUMBER = 1;
  SSH1_PROTOFLAGS_SUPPORTED = 0;


  SSH2_MSG_DISCONNECT = 1;
  SSH2_MSG_IGNORE = 2;
  SSH2_MSG_UNIMPLEMENTED = 3;
  SSH2_MSG_DEBUG = 4;
  SSH2_MSG_SERVICE_REQUEST = 5;
  SSH2_MSG_SERVICE_ACCEPT = 6;
  SSH2_MSG_KEXINIT = 20;
  SSH2_MSG_NEWKEYS = 21;
  SSH2_MSG_KEXDH_INIT = 30;
  SSH2_MSG_KEXDH_REPLY = 31;
  SSH2_MSG_KEX_DH_GEX_REQUEST_OLD = 30;
  SSH2_MSG_KEX_DH_GEX_GROUP = 31;
  SSH2_MSG_KEX_DH_GEX_INIT = 32;
  SSH2_MSG_KEX_DH_GEX_REPLY = 33;
  SSH2_MSG_KEX_DH_GEX_REQUEST = 34;
  SSH2_MSG_USERAUTH_REQUEST = 50;
  SSH2_MSG_USERAUTH_FAILURE = 51;
  SSH2_MSG_USERAUTH_SUCCESS = 52;
  SSH2_MSG_USERAUTH_BANNER = 53;
  SSH2_MSG_USERAUTH_PK_OK = 60;
  SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ = 60;
  SSH2_MSG_USERAUTH_INFO_REQUEST = 60;
  SSH2_MSG_USERAUTH_INFO_RESPONSE = 61;
  SSH2_MSG_GLOBAL_REQUEST = 80;
  SSH2_MSG_REQUEST_SUCCESS = 81;
  SSH2_MSG_REQUEST_FAILURE = 82;
  SSH2_MSG_CHANNEL_OPEN = 90;
  SSH2_MSG_CHANNEL_OPEN_CONFIRMATION = 91;
  SSH2_MSG_CHANNEL_OPEN_FAILURE = 92;
  SSH2_MSG_CHANNEL_WINDOW_ADJUST = 93;
  SSH2_MSG_CHANNEL_DATA = 94;
  SSH2_MSG_CHANNEL_EXTENDED_DATA = 95;
  SSH2_MSG_CHANNEL_EOF = 96;
  SSH2_MSG_CHANNEL_CLOSE = 97;
  SSH2_MSG_CHANNEL_REQUEST = 98;
  SSH2_MSG_CHANNEL_SUCCESS = 99;
  SSH2_MSG_CHANNEL_FAILURE = 100;
  SSH2_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT = 1;
  SSH2_DISCONNECT_PROTOCOL_ERROR = 2;
  SSH2_DISCONNECT_KEY_EXCHANGE_FAILED = 3;
  SSH2_DISCONNECT_HOST_AUTHENTICATION_FAILED = 4;
  SSH2_DISCONNECT_RESERVED = 4;
  SSH2_DISCONNECT_MAC_ERROR = 5;
  SSH2_DISCONNECT_COMPRESSION_ERROR = 6;
  SSH2_DISCONNECT_SERVICE_NOT_AVAILABLE = 7;
  SSH2_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED = 8;
  SSH2_DISCONNECT_HOST_KEY_NOT_VERIFIABLE = 9;
  SSH2_DISCONNECT_CONNECTION_LOST = 10;
  SSH2_DISCONNECT_BY_APPLICATION = 11;
  SSH2_DISCONNECT_TOO_MANY_CONNECTIONS = 12;
  SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER = 13;
  SSH2_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE = 14;
  SSH2_DISCONNECT_ILLEGAL_USER_NAME = 15;
  SSH2_OPEN_ADMINISTRATIVELY_PROHIBITED = 1;
  SSH2_OPEN_CONNECT_FAILED = 2;
  SSH2_OPEN_UNKNOWN_CHANNEL_TYPE = 3;
  SSH2_OPEN_RESOURCE_SHORTAGE = 4;
  SSH2_EXTENDED_DATA_STDERR = 1;


implementation

end.
