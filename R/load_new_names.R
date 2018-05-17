# load_new_names <- function(){
# 
#     # alias
#   make.selection.table <- selection_table
#   auto_detec <- autodetec
#   check_wavs <- checkwavs
#   compare_methods <- compare.methods
#   coor_test <- coor.test
#   coor_graph <- coor.graph
#   df_DTW <- dfDTW
#   df_ts <- dfts
#   ff_DTW <- ffDTW
#   ff_ts <- ffts
#   filter_sels <- filtersels
#   fix_wavs <- fixwavs
#   freq_range <- frange
#   freq_range_detec <- frange.detec
#   manual_loc <- manualoc
#   move_imgs <- move.imgs
#   sel_tailor <- seltailor
#   se_ts <- sp.en.ts
#   snr_specs <- snrspecs
#   spec_an <- specan
#   spectrograms <- specreator
#   track_freqs <- trackfreqs
#   color_spectro <- color.spectro
#   xc_maps <- xcmaps
#   quer_xc <- querxc
#   check_sels <- checksels
#   
#   
#   name_changes <- data.frame('New names' = c("auto_detec", "check_sels", "check_wavs", "compare_methods", "coor_test", "coor_graph", "df_DTW", "df_ts", "ff_ts", "filter_sels", "fix_wavs", "freq_range", "freq_range_detec", "manual_loc", "move_imgs", "sel_tailor","se_ts", "snr_specs", "spec_an", "spectrograms", "track_freqs", "color_spectro", "xc_maps", "quer_xc"),
#                              'Old names'= c("autodetec", "checksels", "checkwavs", "compare.methods", "coor.test", "coor.graph", "dfDTW", "dfts", "ffts", "filtersels", "fixwavs", "frange", "frange.detec", "manualoc", "move.imgs", "seltailor", "sp.en.ts", "snrspecs", "specan", "specreator", "trackfreqs", "color.spectro", "xcmaps", "querxc"))
#   
#   nl <- list(make.selection.table = make.selection.table, auto_detec = auto_detec, check_sels = check_sels, check_wavs = check_wavs, compare_methods = compare_methods, coor_test = coor_test, coor_graph = coor_graph, df_DTW = df_DTW, df_ts = df_ts, ff_ts = ff_ts, filter_sels = filter_sels, fix_wavs = fix_wavs, freq_range = freq_range, freq_range_detec = freq_range_detec, manual_loc = manual_loc, move_imgs = move_imgs, sel_tailor = sel_tailor, se_ts = se_ts, snr_specs = snr_specs, spec_an = spec_an, spectrograms = spectrograms, track_freqs = track_freqs, color_spectro = color_spectro, xc_maps = xc_maps, quer_xc = quer_xc, name_changes = name_changes)
# 
#   attach(nl)
# 
# }
# 
# 
# rm_new_names <- function(){
#   
#   nl <- c(make.selection.table, auto_detec, check_sels, check_wavs, compare_methods, coor_test, coor_graph, df_DTW, df_ts, ff_ts, filter_sels, fix_wavs, freq_range, freq_range_detec, manual_loc, move_imgs, sel_tailor, se_ts, snr_specs, spec_an , spectrograms, track_freqs, color_spectro, xc_maps, quer_xc, name_changes)
#   
#   rm(nl)
# }

