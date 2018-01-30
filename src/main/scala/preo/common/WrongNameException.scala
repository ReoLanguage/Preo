package preo.common

class WrongNameException(message: String , private val cause: Throwable = None.orNull)
  extends Exception(message, cause)