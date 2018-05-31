-define(FLAG_BOUNDARY, 16#3E).
-define(ESCAPED_3E, <<16#3D, 16#02>>).
-define(ESCAPED_3D, <<16#3D, 16#01>>).


%% -record(jt808_frame_options,
%%         {boundary = ?FLAG_BOUNDARY}).

-record(jt808_frame, {header, payload}).

-record(jt808_frame_header,
        {id,
         aes     = 0,
         zip     = 0,
         divide  = 0,
         length,
         timestamp,
         sn}).
