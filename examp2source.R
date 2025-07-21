atls_examp_dir <- "C:/repos/CVPAS_STH_app/Front end/App/mods"

# atlas examp
mod_dirs <- dir(atls_examp_dir)
mod_dirs_ls <- lapply(1:length(mod_dirs),function(x) dir(file.path(atls_examp_dir,mod_dirs[x]))); names(mod_dirs_ls) <- mod_dirs

comb_serv_scripts <- c(
  # mod1_dirs=
  file.path(atls_examp_dir,"TagLife_module",
            c("reactives_all.R",
              "TL_input_rUI.R",
              "TL_data_plot_modal_rUI.R",
              "TL_lock_data_rUI.R",
              "TL_GOF_table_modal_rUI.R",
              "TL_fit_plot_modal_rUI.R")),
  file.path(atls_examp_dir,"unadj_CJS_module",
            c("single/CJS_input_rUI.R",
              "single/CJS_data_plot_modal_rUI.R",
              "single/CJS_lock_data_rUI.R",
              "paired/placeholder.R")),
  file.path(atls_examp_dir,"unadj_CJS_module",
            c("single/placeholder.R",
              "paired/placeholder.R")))


# transferring files
sapply(seq_along(comb_serv_scripts),function(ii){
  file.copy(from = comb_serv_scripts[ii],
            to=file.path(my_dir,"server_elements"),
            overwrite = TRUE )})

print(all(sapply(comb_serv_scripts,file.exists)))