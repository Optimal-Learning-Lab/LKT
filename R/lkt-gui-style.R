# A shared presentation layer for fgui forms. fgui owns values; this layer owns
# layout, themed controls and action/error presentation. No model is run on edit.
lkt_gui_palette <- function(theme) {
  theme<-match.arg(theme,c('blue','green'))
  list(background=if(theme=='blue')'#F2F7FB' else '#F1F8F5',surface='#FFFFFF',
    ink='#19384B',muted='#526978',accent=if(theme=='blue')'#286B86' else '#23765D',
    active=if(theme=='blue')'#20556B' else '#1B5C49',border='#D6E2EA')
}

lkt_tk_children <- function(widget) {
  x<-as.character(tcltk::tcl('winfo','children',widget));x[nzchar(x)]
}

lkt_gui_style_tree <- function(widget,palette) {
  type<-as.character(tcltk::tcl('winfo','class',widget))
  configure<-function(...)tcltk::tcl(widget,'configure',...)
  if(type%in%c('Toplevel','Frame','Labelframe'))configure('-background',palette$surface)
  if(type=='Frame')configure('-relief','flat','-borderwidth',0)
  if(type=='Label')configure('-background',palette$surface,'-foreground',palette$muted,'-font','TkDefaultFont')
  if(type=='Entry')configure('-background',palette$surface,'-foreground',palette$ink,
    '-relief','solid','-borderwidth',1,'-highlightthickness',1,'-highlightbackground',palette$border,
    '-highlightcolor',palette$accent,'-font','TkTextFont')
  if(type=='Text')configure('-background',palette$surface,'-foreground',palette$ink,
    '-relief','flat','-borderwidth',0,'-font','TkFixedFont','-padx',8,'-pady',6)
  if(type=='Button')configure('-background',palette$background,'-foreground',palette$accent,
    '-activebackground',palette$border,'-relief','flat','-borderwidth',0,'-font','TkDefaultFont','-padx',8,'-pady',5)
  if(type=='Scrollbar')configure('-background',palette$background,'-troughcolor',palette$surface,'-relief','flat','-borderwidth',0)
  for(child in lkt_tk_children(widget))lkt_gui_style_tree(child,palette)
}

lkt_gui_theme <- function(window,theme) {
  p<-lkt_gui_palette(theme)
  tcltk::tcl('ttk::style','theme','use','clam')
  fonts<-as.character(tcltk::tcl('font','names'))
  family<-if(.Platform$OS.type=='windows')'Segoe UI' else 'Helvetica'
  for(font in intersect(fonts,c('TkDefaultFont','TkTextFont','TkMenuFont','TkHeadingFont')))
    tcltk::tcl('font','configure',font,'-family',family,'-size',11)
  if('TkFixedFont'%in%fonts)tcltk::tcl('font','configure','TkFixedFont','-family',
    if(.Platform$OS.type=='windows')'Consolas' else 'Courier','-size',10)
  tcltk::tcl('ttk::style','configure','LKT.TFrame','-background',p$surface)
  tcltk::tcl('ttk::style','configure','LKT.TNotebook','-background',p$background,'-borderwidth',0)
  tcltk::tcl('ttk::style','configure','LKT.TNotebook.Tab','-padding','16 8','-font','TkDefaultFont',
    '-background',p$background,'-foreground',p$muted)
  tcltk::tcl('ttk::style','map','LKT.TNotebook.Tab','-background',c('selected',p$surface),
    '-foreground',c('selected',p$ink))
  tcltk::tcl('ttk::style','configure','LKT.TCombobox','-padding',5,'-font','TkTextFont','-fieldbackground',p$surface)
  tcltk::tcl('ttk::style','map','LKT.TCombobox','-fieldbackground',c('readonly',p$surface),
    '-foreground',c('readonly',p$ink),'-selectbackground',c('readonly',p$surface),'-selectforeground',c('readonly',p$ink))
  tcltk::tcl('ttk::style','configure','LKT.TButton','-padding','12 7','-font','TkDefaultFont',
    '-background',p$background,'-foreground',p$accent,'-borderwidth',1)
  tcltk::tcl('ttk::style','configure','LKT.Primary.TButton','-padding','18 8','-font','TkDefaultFont',
    '-background',p$accent,'-foreground','#FFFFFF','-borderwidth',0)
  tcltk::tcl('ttk::style','map','LKT.Primary.TButton','-background',c('active',p$active))
  lkt_gui_style_tree(window,p)
  tcltk::tkconfigure(window,background=p$background)
  p
}

#' Present a themed fgui configuration form
#'
#' @param func Function whose scalar defaults define the fields. It is called
#'   only when the user presses the action button, with current field values.
#' @param title Window title.
#' @param subtitle Short instructions above the form.
#' @param sections Named list of field-name vectors, one per notebook tab.
#' @param choices Named list of allowed values for read-only dropdowns.
#' @param labels Named list of field labels.
#' @param edit Named fgui multiline-editor specifications.
#' @param files Character vector of file-browser fields.
#' @param buttons Named list of additional explicit action callbacks.
#' @param submit_label Primary button text.
#' @param modal Wait for the form to close.
#' @param theme blue or green.
#' @return Initial form values invisibly for a nonmodal form; otherwise NULL.
#' @export
lkt_form_gui <- function(func,title='LKT',subtitle='',sections=NULL,choices=list(),labels=list(),
                         edit=list(),files=character(),buttons=list(),submit_label='Apply action',
                         modal=TRUE,theme=c('blue','green')) {
  theme<-match.arg(theme)
  if(!requireNamespace('fgui',quietly=TRUE))stop('Install the optional fgui package to open the interface.')
  fields<-names(formals(func))
  # fgui does not initialize integer scalar defaults as numeric entries.
  # Present them as doubles; controllers validate and restore integer types.
  for(name in fields)if(is.integer(formals(func)[[name]])&&length(formals(func)[[name]])==1L)
    formals(func)[[name]]<-as.numeric(formals(func)[[name]])
  # Tcl represents logical defaults as 1/0; named choices use their visible
  # TRUE/FALSE labels. Preserve the specified logical value as a string.
  for(name in intersect(names(choices),fields))if(is.logical(formals(func)[[name]]))
    formals(func)[[name]]<-as.character(formals(func)[[name]])
  if(modal) {
    keys<-c('argList','argListSelected','func','object','guiObject','fargTypes','fargNames','farg','output','MAIN',
      'GUIINTERNALS_getFix','GUIINTERNALS_cancelled')
    previous<-lapply(keys,function(key)tryCatch(fgui::guiGet(key),error=function(e)NULL));names(previous)<-keys
    on.exit(for(key in keys)fgui::guiSet(key,previous[[key]]),add=TRUE)
  }
  invisible(fgui::gui(func,title=title,modal=FALSE,output='m',getFix=FALSE,
    exec=NULL,cancelButton=FALSE,argGridFrame=rep('g',length(fields)),
    argEdit=edit,argFilename=stats::setNames(as.list(rep('',length(files))),files),argText=labels))
  window<-fgui::guiGet('MAIN');objects<-fgui::guiGet('object');controls<-fgui::guiGet('guiObject')
  p<-lkt_gui_theme(window,theme)
  for(name in intersect(names(edit),fields)) {
    i<-match(name,fields);default<-formals(func)[[name]]
    if(is.character(default))tcltk::tkinsert(objects[[i]],'1.0',default)
    tcltk::tkconfigure(objects[[i]],wrap='none')
    tcltk::tkgrid.configure(objects[[i]],sticky='nsew')
    local({
      editor<-objects[[i]];frame<-controls[[i]]
      bar<-tcltk::ttkscrollbar(frame,orient='horizontal',command=function(...)tcltk::tkxview(editor,...))
      tcltk::tkconfigure(editor,xscrollcommand=function(...)tcltk::tkset(bar,...))
      tcltk::tkgrid(bar,row=2,column=0,sticky='ew')
      tcltk::tkgrid.columnconfigure(frame,0,weight=1);tcltk::tkgrid.rowconfigure(frame,1,weight=1)
    })
  }
  # Each field is a root child (fgui grid frame g), so grid -in may manage it
  # inside a descendant tab without reparenting or duplicating its state.
  for(name in names(choices)) {
    i<-match(name,fields)
    if(is.na(i))stop('Unknown dropdown field: ',name)
    current<-as.character(tcltk::tclvalue(objects[[i]]))
    if(!current%in%choices[[name]])stop('Initial dropdown value is not an allowed choice: ',name)
    tcltk::tkdestroy(controls[[i]])
    frame<-tcltk::ttkframe(window,style='LKT.TFrame',padding=4)
    label<-if(name%in%names(labels))labels[[name]] else gsub('_',' ',name,fixed=TRUE)
    tcltk::tkgrid(tcltk::ttklabel(frame,text=label,background=p$surface,foreground=p$muted),row=0,column=0,padx=5,sticky='w')
    combo<-tcltk::ttkcombobox(frame,textvariable=objects[[i]],values=choices[[name]],state='readonly',
      style='LKT.TCombobox',width=30)
    tcltk::tkgrid(combo,row=0,column=1,padx=8,sticky='ew');tcltk::tkgrid.columnconfigure(frame,1,weight=1)
    controls[[i]]<-frame
  }
  fgui::guiSet('guiObject',controls)
  # Remove the initial grid before creating the application shell.
  for(child in lkt_tk_children(window))tcltk::tcl('grid','forget',child)
  header<-tcltk::ttkframe(window,padding=16,style='LKT.TFrame')
  tcltk::tkgrid(header,row=0,column=0,sticky='ew',padx=16,pady=c(16,8))
  tcltk::tkgrid(tcltk::ttklabel(header,text=title,font=c(if(.Platform$OS.type=='windows')'Segoe UI' else 'Helvetica',18,'bold'),
    background=p$surface,foreground=p$ink),sticky='w')
  tcltk::tkgrid(tcltk::ttklabel(header,text=subtitle,wraplength=920,background=p$surface,foreground=p$muted),sticky='w',pady=c(6,0))
  notebook<-tcltk::ttknotebook(window,style='LKT.TNotebook')
  tcltk::tkgrid(notebook,row=1,column=0,sticky='nsew',padx=16,pady=8)
  if(is.null(sections))sections<-list(Settings=setdiff(fields,'action'))
  assigned<-unlist(sections,use.names=FALSE)
  if(anyDuplicated(assigned)||!setequal(assigned,setdiff(fields,'action')))
    stop('Form tabs must cover each non-action field exactly once.')
  for(section in names(sections)) {
    tab<-tcltk::ttkframe(notebook,padding=12,style='LKT.TFrame')
    tcltk::tkadd(notebook,tab,text=section)
    for(row in seq_along(sections[[section]])) {
      name<-sections[[section]][row];i<-match(name,fields)
      tcltk::tkgrid(controls[[i]],'in'=tab,row=row-1L,column=0,sticky=if(name%in%names(edit))'nsew' else 'ew',pady=5)
      tcltk::tkraise(controls[[i]])
      if(name%in%names(edit))tcltk::tkgrid.rowconfigure(tab,row-1L,weight=1)
    }
    tcltk::tkgrid.columnconfigure(tab,0,weight=1)
  }
  footer<-tcltk::ttkframe(window,padding=8,style='LKT.TFrame')
  tcltk::tkgrid(footer,row=2,column=0,sticky='ew',padx=16,pady=6)
  output<-objects[[length(objects)]]
  tcltk::tkconfigure(output,height=7,width=85,wrap='word')
  output_control<-controls[[length(controls)]]
  tcltk::tkgrid.configure(output,sticky='nsew')
  tcltk::tkgrid.columnconfigure(output_control,0,weight=1)
  tcltk::tkgrid.rowconfigure(output_control,1,weight=1)
  tcltk::tkgrid(output_control,row=3,column=0,sticky='ew',padx=16,pady=c(4,16))
  tcltk::tkgrid.columnconfigure(window,0,weight=1);tcltk::tkgrid.rowconfigure(window,1,weight=1)
  show<-function(value) {
    tcltk::tkdelete(output,'1.0','end')
    tcltk::tkinsert(output,'1.0',paste(as.character(value),collapse='\n'))
  }
  run<-function(callback) {
    warnings<-character()
    tcltk::tkconfigure(window,cursor='watch');tcltk::tcl('update','idletasks')
    result<-tryCatch(withCallingHandlers(callback(),warning=function(w) {
      warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')
    }),error=function(e)paste('Unable to complete action:',conditionMessage(e)))
    if(as.integer(tcltk::tcl('winfo','exists',window))==1L) {
      tcltk::tkconfigure(window,cursor='')
      if(!is.null(result)||length(warnings))show(c(result,if(length(warnings))c('\nWarnings:',unique(warnings))))
    }
    invisible(result)
  }
  col<-0L
  if('action'%in%fields) {
    tcltk::tkgrid(controls[[match('action',fields)]],'in'=footer,row=0,column=col,sticky='ew');col<-col+1L
    tcltk::tkraise(controls[[match('action',fields)]])
    tcltk::tkgrid.columnconfigure(footer,0,weight=1)
  }
  for(name in names(buttons)) {
    callback<-buttons[[name]]
    button<-local({cb<-callback;tcltk::ttkbutton(footer,text=name,style='LKT.TButton',command=function()run(cb))})
    tcltk::tkgrid(button,row=0,column=col,padx=5);col<-col+1L
  }
  apply<-tcltk::ttkbutton(footer,text=submit_label,style='LKT.Primary.TButton',
    command=function()run(function()do.call(func,fgui::guiGetAllValues())))
  tcltk::tkgrid(apply,row=0,column=col,padx=5)
  tcltk::tkgrid(tcltk::ttkbutton(footer,text='Close',style='LKT.TButton',command=function()tcltk::tkdestroy(window)),row=0,column=col+1L,padx=5)
  width<-min(1100,as.integer(tcltk::tkwinfo('screenwidth',window))-100)
  height<-min(850,as.integer(tcltk::tkwinfo('screenheight',window))-140)
  tcltk::tkwm.geometry(window,paste0(width,'x',height));tcltk::tkwm.minsize(window,850,620)
  show(c('Choose an action, then press Apply. Editing settings does not run a model or submit a simulation.',
    if('output_file'%in%fields)paste('Output file:',fgui::guiGetAllValues()$output_file)))
  if(modal){tcltk::tkwait.window(window);return(invisible(NULL))}
  invisible(fgui::guiGetAllValues())
}
