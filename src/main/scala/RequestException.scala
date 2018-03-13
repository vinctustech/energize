package xyz.hyperreal.energize


class RequestException( error: String ) extends Exception( error )

class NotFoundRequestException( error: String ) extends RequestException( error )

class BadRequestRequestException( error: String ) extends RequestException( error )

class ForbiddenRequestException( error: String ) extends RequestException( error )

class UnauthorizedRequestException( realm: String ) extends RequestException( realm )

class ExpiredRequestException( realm: String ) extends RequestException( realm )
