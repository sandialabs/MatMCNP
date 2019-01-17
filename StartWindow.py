import os
from traits.api import HasTraits, Bool, Button, File, Property
from traitsui.api import View, Item, Group, FileEditor, Handler, UItem, ListEditor
from traits.etsconfig.api import ETSConfig
ETSConfig.toolkit = 'qt4'



class ClosingHandler(Handler):
    def object_cont_changed(self, info):
        info.ui.dispose()
        tmpfile = open('tmpfile', 'w')
        tmpfile.write(str(info.object.new_file)+'\n')
        if info.object.new_file == False:
            tmpfile.write(str(info.object.existing_file)) 
        tmpfile.close() 
        os._exit(0)

class Start_File(HasTraits):
    new_file = Bool
    existing_file = File(False, editor = FileEditor(filter = ['*.pkl']))
    cont = Button('Continue')
    
view2 = View(
              Group(
                     Item(name = 'new_file', springy = False,
                          tooltip = "Clicking this button, followed by \"Continue\" will bring up a blank interface to start a new material."),
                     Item(name = 'existing_file', style = 'custom', height = -500, springy = False,
                          tooltip = "Use this file browser to find a previously saved .pkl file. Click on the file, followed by \"Continue\" to "\
                                    "resume the calculation from the previously saved state."),
                     UItem('cont',
                           tooltip = "Click this button to proceed to the next window.",
                           enabled_when = '(existing_file) or (new_file)')
                   ), 
              scrollable = True, resizable = True, width = 1000, height = 800, title = 'MatMCNP', handler=ClosingHandler(),
            )



if __name__ == '__main__':
  Begin = Start_File()
  Begin.configure_traits(view=view2)
