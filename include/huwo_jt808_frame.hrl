-define(FLAG_BOUNDARY, 16#3E).
-define(ESCAPED_3E, <<16#3D, 16#02>>).
-define(ESCAPED_3D, <<16#3D, 16#01>>).

-define(MSG_ID_REG,16#0100).
-define(MSG_ID_HW_REG,16#0103).

%% -record(huwo_jt808_frame_options,
%%         {boundary = ?FLAG_BOUNDARY}).

-record(huwo_jt808_frame, {header, payload}).

-record(huwo_jt808_frame_header,
        {id,
         aes     = 0,
         zip     = 0,
         divide  = 0,
         length,
         timestamp,
         sn}).
