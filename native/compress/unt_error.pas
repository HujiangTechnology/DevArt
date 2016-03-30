unit unt_error;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  ERROR_NONE = 0;
  ERROR_FORMAT_NOT_SUPPORT = -1;
  ERROR_UNCOMPRESS = -2;
  ERROR_COMPRESS = -3;

  ERRMSG_NONE = 'no error found';
  ERRMSG_FORMAT_NOT_SUPPORT = 'format not support';
  ERRMSG_UNCOMPRESS = 'uncompress error';
  ERRMSG_COMPRESS = 'compress error';

var
  ERROR_CODE: Integer = 0;
  ERROR_MESSAGE: string = '';

implementation

end.

