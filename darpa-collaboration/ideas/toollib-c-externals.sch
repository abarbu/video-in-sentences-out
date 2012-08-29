;;; LaHaShem HaAretz U'Mloah

;;; standard libraries

(define-c-external (sleep int) void "sleep")

;;; tool1lib-c.c

(define-c-external (make-c-int-vector int) pointer "make_c_int_vector")

(define-c-external (c-int-vector-ref pointer int) int "c_int_vector_ref")

(define-c-external (c-int-vector-set! pointer int int) void "c_int_vector_set")

(define-c-external (free-c-int-vector! pointer) void "free_c_int_vector")

(define-c-external (make-c-int-matrix int int) pointer "make_c_int_matrix")

(define-c-external (c-int-matrix-ref pointer int int) int "c_int_matrix_ref")

(define-c-external (c-int-matrix-set! pointer int int int) void
 "c_int_matrix_set")

(define-c-external (free-c-int-matrix! pointer int) void "free_c_int_matrix")

(define-c-external (create-image unsigned unsigned) pointer "create_image")

(define-c-external (viff-set-pixel! pointer unsigned unsigned unsigned) void
 "viff_set_pixel")

(define-c-external (detect-edges! float float int int int int pointer) void
 "detect_edges")

(define-c-external (detect-line-segments! float float int int int int pointer)
 void "detect_line_segments")

(define-c-external (edge-pixel pointer int int) int "edge_pixel")

(define-c-external (number-of-line-segments) int "number_of_line_segments")

(define-c-external (line-segment-x1 int) int "line_segment_x1")

(define-c-external (line-segment-y1 int) int "line_segment_y1")

(define-c-external (line-segment-x2 int) int "line_segment_x2")

(define-c-external (line-segment-y2 int) int "line_segment_y2")

(define-c-external (free-xvimage! pointer) void "free_xvimage")

(define-c-external (wall-clock) int "wall_clock")

;;; tool1lib-v4l-c.c

(define-c-external (v4l-set-video-device! int pointer) void
 "v4l_set_video_device")

(define-c-external (v4l-available-internal int) int "v4l_available")

(define-c-external (v4l-open-video! int) void "v4l_open_video")

(define-c-external (v4l-close-video! int) void "v4l_close_video")

(define-c-external (v4l-restore-factory-settings! int) void
 "v4l_restore_factory_settings")

(define-c-external (v4l-save-user-settings! int) void "v4l_save_user_settings")

(define-c-external (v4l-restore-user-settings! int) void
 "v4l_restore_user_settings")

(define-c-external (v4l-get-agc int) int "v4l_get_agc")

(define-c-external (v4l-set-agc! int int) void "v4l_set_agc")

(define-c-external (v4l-set-shutter! int int) void "v4l_set_agc")

(define-c-external (v4l-get-compression int) int "v4l_get_compression")

(define-c-external (v4l-set-compression! int int) void "v4l_set_compression")

(define-c-external (v4l-get-palette int) int "v4l_get_palette")

(define-c-external (v4l-set-palette! int int int) void "v4l_set_palette")

(define-c-external (v4l-get-brightness int) int "v4l_get_brightness")

(define-c-external (v4l-set-brightness! int int) void "v4l_set_brightness")

(define-c-external (v4l-get-contrast int) int "v4l_get_contrast")

(define-c-external (v4l-set-contrast! int int) void "v4l_set_contrast")

(define-c-external (v4l-get-hue int) int "v4l_get_hue")

(define-c-external (v4l-set-hue! int int) void "v4l_set_hue")

(define-c-external (v4l-get-colour int) int "v4l_get_colour")

(define-c-external (v4l-set-colour! int int) void "v4l_set_colour")

(define-c-external (v4l-get-whiteness int) int "v4l_get_whiteness")

(define-c-external (v4l-set-whiteness! int int) void "v4l_set_whiteness")

(define-c-external (v4l-get-width int) int "v4l_get_width")

(define-c-external (v4l-get-height int) int "v4l_get_height")

(define-c-external (v4l-set-size! int int int) void "v4l_set_size")

(define-c-external (v4l-get-frame-rate int) int "v4l_get_frame_rate")

(define-c-external (v4l-set-frame-rate! int int int) void "v4l_set_frame_rate")

(define-c-external (v4l-get-maxwidth int) int "v4l_get_maxwidth")

(define-c-external (v4l-get-maxheight int) int "v4l_get_maxheight")

(define-c-external (v4l-get-minwidth int) int "v4l_get_minwidth")

(define-c-external (v4l-get-minheight int) int "v4l_get_minheight")

(define-c-external (v4l-get-channels int) int "v4l_get_channels")

(define-c-external (v4l-get-name int) pointer "v4l_get_name")

(define-c-external (v4l-allocate-frame! int) void "v4l_allocate_frame")

(define-c-external (v4l-free-frame! int) void "v4l_free_frame")

(define-c-external (v4l-frame-available int) int "v4l_frame_available")

(define-c-external (v4l-get-frame! int) void "v4l_get_frame")

(define-c-external (v4l-mmap-frame! int) void "v4l_mmap_frame")

(define-c-external (v4l-munmap-frame! int) void "v4l_munmap_frame")

(define-c-external (v4l-get-number-of-mmaped-frames int) int
 "v4l_get_number_of_mmaped_frames")

(define-c-external (v4l-get-mmap-capture! int int) void "v4l_mmap_capture")

(define-c-external (v4l-get-mmap-sync! int int) void "v4l_mmap_sync")

(define-c-external (v4l-get-mmap-get-frame! int) void "v4l_mmap_get_frame")

(define-c-external (v4l-interrupt-on! int) void "v4l_interrupt-on")

(define-c-external (v4l-interrupt-off!) void "v4l_interrupt-off")

(define-c-external (v4l-write-ppm! int pointer) void "v4l_write_ppm")

(define-c-external (v4l-home! int int int) void "v4l_home")

(define-c-external (v4l-get-pan-min int) int "v4l_get_pan_min")

(define-c-external (v4l-get-pan-max int) int "v4l_get_pan_max")

(define-c-external (v4l-get-tilt-min int) int "v4l_get_tilt_min")

(define-c-external (v4l-get-tilt-max int) int "v4l_get_tilt_max")

(define-c-external (v4l-get-pan int) int "v4l_get_pan")

(define-c-external (v4l-get-tilt int) int "v4l_get_tilt")

(define-c-external (v4l-set-pan-tilt! int int int int) void "v4l_set_pan_tilt")

(define-c-external (v4l-pan-tilt-status int) int "v4l_pan_tilt_status")

;;; tool1lib-hack-track-c.c

(define-c-external (get-first-frame int) int "get_first_frame")

(define-c-external (get-last-frame int) int "get_last_frame")

(define-c-external (get-object-k int int) int "get_object_k")

(define-c-external (get-ellipse-x int int int) double "get_ellipse_x")

(define-c-external (get-ellipse-y int int int) double "get_ellipse_y")

(define-c-external (get-ellipse-theta int int int) double "get_ellipse_theta")

(define-c-external (get-ellipse-r int int int) double "get_ellipse_r")

(define-c-external (get-ellipse-s int int int) double "get_ellipse_s")

;;; Tam V'Nishlam Shevah L'El Borei Olam
