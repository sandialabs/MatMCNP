import subprocess
import os
import pickle
import numpy as np
from traits.api import HasTraits, Enum, Bool, Str, Int, Float, Button, List, Event, File, Directory
from traitsui.api import View, Item, Group, TextEditor, spring, Handler, UItem, FileEditor, DirectoryEditor, EnumEditor

class ClosingHandler(Handler):

    def object_save_file_changed(self, info):
        pickle_file = open(info.object.file_location+'/'+info.object.file_name+'.pkl', 'wb')
        pickle.dump(info.object, pickle_file)
        pickle_file.close()

    def object_run_matmcnp_changed(self, info):
        ############################### open the MatMCNP input file ####################################
        inp_file = open(info.object.file_location+'/'+info.object.file_name+'.inp', 'w')
        ###################################### print the title #########################################
        inp_file.write(info.object.title+'\n')
        #################################### print the comments ########################################
        lines = info.object.comments.split('\n')
        N = len(lines)+2
        inp_file.write('%-4d\n' % (N))
        inp_file.write('**1\n')
        nlines = 1
        for i in range(len(lines)):
            nlines += 1
            inp_file.write("{:*>3d}".format(nlines)+'  '+'%-65s' % (lines[i])+'\n')
        inp_file.write("{:*>3d}".format(nlines+1)+'\n')
        #################################### print the density #########################################
        inp_file.write('%-10.5f\n' % (info.object.phys_dens))
        ################################# print the fraction type ######################################
        if info.object.frac_type == "Atomic":
            inp_file.write('atomic\n')
        elif info.object.frac_type == "Weight":
            inp_file.write('weight\n')
        ############################### print the number of elements ###################################
        inp_file.write('%-4d\n' % (info.object.num_ele))
        ele_dict = {"1-H": 1,"2-He": 2,"3-Li": 3,"4-Be": 4,"5-B": 5,"6-C": 6,"7-N": 7,"8-O": 8,"9-F": 9,"10-Ne": 10,"11-Na": 11,"12-Mg": 12,"13-Al": 13,"14-Si": 14,"15-P": 15,"16-S": 16,"17-Cl": 17,"18-Ar": 18,"19-K": 19,"20-Ca": 20,"21-Sc": 21,"22-Ti": 22,"23-V": 23,"24-Cr": 24,"25-Mn": 25,"26-Fe": 26,"27-Co": 27,"28-Ni": 28,"29-Cu": 29,"30-Zn": 30,"31-Ga": 31,"32-Ge": 32,"33-As": 33,"34-Se": 34,"35-Br": 35,"36-Kr": 36,"37-Rb": 37,"38-Sr": 38,"39-Y": 39,"40-Zr": 40,"41-Nb": 41,"42-Mo": 42,"44-Ru": 44,"45-Rh": 45,"46-Pd": 46,"47-Ag": 47,"48-Cd": 48,"49-In": 49,"50-Sn": 50,"51-Sb": 51,"52-Te": 52,"53-I": 53,"54-Xe": 54,"55-Cs": 55,"56-Ba": 56,"57-La": 57,"58-Ce": 58,"59-Pr": 59,"60-Nd": 60,"62-Sm": 62,"63-Eu": 63,"64-Gd": 64,"65-Tb": 65,"66-Dy": 66,"67-Ho": 67,"68-Er": 68,"69-Tm": 69,"70-Yb": 70,"71-Lu": 71,"72-Hf": 72,"73-Ta": 73,"74-W": 74,"75-Re": 75,"76-Os": 76,"77-Ir": 77,"78-Pt": 78,"79-Au": 79,"80-Hg": 80,"81-Tl": 81,"82-Pb": 82,"83-Bi": 83,"90-Th": 90,"92-U": 92}
        elements = [ele_dict[info.object.ele1], ele_dict[info.object.ele2], ele_dict[info.object.ele3], ele_dict[info.object.ele4], ele_dict[info.object.ele5], ele_dict[info.object.ele6], ele_dict[info.object.ele7], ele_dict[info.object.ele8], ele_dict[info.object.ele9], ele_dict[info.object.ele10], ele_dict[info.object.ele11], ele_dict[info.object.ele12], ele_dict[info.object.ele13], ele_dict[info.object.ele14], ele_dict[info.object.ele15], ele_dict[info.object.ele16], ele_dict[info.object.ele17], ele_dict[info.object.ele18], ele_dict[info.object.ele19], ele_dict[info.object.ele20], ele_dict[info.object.ele21], ele_dict[info.object.ele22], ele_dict[info.object.ele23], ele_dict[info.object.ele24], ele_dict[info.object.ele25], ele_dict[info.object.ele26], ele_dict[info.object.ele27], ele_dict[info.object.ele28], ele_dict[info.object.ele29], ele_dict[info.object.ele30], ele_dict[info.object.ele31], ele_dict[info.object.ele32], ele_dict[info.object.ele33], ele_dict[info.object.ele34], ele_dict[info.object.ele35], ele_dict[info.object.ele36], ele_dict[info.object.ele37], ele_dict[info.object.ele38], ele_dict[info.object.ele39], ele_dict[info.object.ele40], ele_dict[info.object.ele41], ele_dict[info.object.ele42], ele_dict[info.object.ele43], ele_dict[info.object.ele44], ele_dict[info.object.ele45], ele_dict[info.object.ele46], ele_dict[info.object.ele47], ele_dict[info.object.ele48], ele_dict[info.object.ele49], ele_dict[info.object.ele50], ele_dict[info.object.ele51], ele_dict[info.object.ele52], ele_dict[info.object.ele53], ele_dict[info.object.ele54], ele_dict[info.object.ele55], ele_dict[info.object.ele56], ele_dict[info.object.ele57], ele_dict[info.object.ele58], ele_dict[info.object.ele59], ele_dict[info.object.ele60], ele_dict[info.object.ele62], ele_dict[info.object.ele62], ele_dict[info.object.ele63], ele_dict[info.object.ele64], ele_dict[info.object.ele65], ele_dict[info.object.ele66], ele_dict[info.object.ele67], ele_dict[info.object.ele68], ele_dict[info.object.ele69], ele_dict[info.object.ele70], ele_dict[info.object.ele71], ele_dict[info.object.ele72], ele_dict[info.object.ele73], ele_dict[info.object.ele74], ele_dict[info.object.ele75], ele_dict[info.object.ele76], ele_dict[info.object.ele77], ele_dict[info.object.ele78], ele_dict[info.object.ele79], ele_dict[info.object.ele80], ele_dict[info.object.ele81], ele_dict[info.object.ele82], ele_dict[info.object.ele83], ele_dict[info.object.ele84], ele_dict[info.object.ele85], ele_dict[info.object.ele86], ele_dict[info.object.ele87], ele_dict[info.object.ele88], ele_dict[info.object.ele89], ele_dict[info.object.ele90], ele_dict[info.object.ele91], ele_dict[info.object.ele92]]
        fractions = [info.object.frac1, info.object.frac2, info.object.frac3, info.object.frac4, info.object.frac5, info.object.frac6, info.object.frac7, info.object.frac8, info.object.frac9, info.object.frac10, info.object.frac11, info.object.frac12, info.object.frac13, info.object.frac14, info.object.frac15, info.object.frac16, info.object.frac17, info.object.frac18, info.object.frac19, info.object.frac20, info.object.frac21, info.object.frac22, info.object.frac23, info.object.frac24, info.object.frac25, info.object.frac26, info.object.frac27, info.object.frac28, info.object.frac29, info.object.frac30, info.object.frac31, info.object.frac32, info.object.frac33, info.object.frac34, info.object.frac35, info.object.frac36, info.object.frac37, info.object.frac38, info.object.frac39, info.object.frac40, info.object.frac41, info.object.frac42, info.object.frac43, info.object.frac44, info.object.frac45, info.object.frac46, info.object.frac47, info.object.frac48, info.object.frac49, info.object.frac50, info.object.frac51, info.object.frac52, info.object.frac53, info.object.frac54, info.object.frac55, info.object.frac56, info.object.frac57, info.object.frac58, info.object.frac59, info.object.frac60, info.object.frac61, info.object.frac62, info.object.frac63, info.object.frac64, info.object.frac65, info.object.frac66, info.object.frac67, info.object.frac68, info.object.frac69, info.object.frac70, info.object.frac71, info.object.frac72, info.object.frac73, info.object.frac74, info.object.frac75, info.object.frac76, info.object.frac77, info.object.frac78, info.object.frac79, info.object.frac80, info.object.frac81, info.object.frac82, info.object.frac83, info.object.frac84, info.object.frac85, info.object.frac86, info.object.frac87, info.object.frac88, info.object.frac89, info.object.frac90, info.object.frac91, info.object.frac92]
        enriched = [info.object.enr1, info.object.enr2, info.object.enr3, info.object.enr4, info.object.enr5, info.object.enr6, info.object.enr7, info.object.enr8, info.object.enr9, info.object.enr10, info.object.enr11, info.object.enr12, info.object.enr13, info.object.enr14, info.object.enr15, info.object.enr16, info.object.enr17, info.object.enr18, info.object.enr19, info.object.enr20, info.object.enr21, info.object.enr22, info.object.enr23, info.object.enr24, info.object.enr25, info.object.enr26, info.object.enr27, info.object.enr28, info.object.enr29, info.object.enr30, info.object.enr31, info.object.enr32, info.object.enr33, info.object.enr34, info.object.enr35, info.object.enr36, info.object.enr37, info.object.enr38, info.object.enr39, info.object.enr40, info.object.enr41, info.object.enr42, info.object.enr43, info.object.enr44, info.object.enr45, info.object.enr46, info.object.enr47, info.object.enr48, info.object.enr49, info.object.enr50, info.object.enr51, info.object.enr52, info.object.enr53, info.object.enr54, info.object.enr55, info.object.enr56, info.object.enr57, info.object.enr58, info.object.enr59, info.object.enr60, info.object.enr61, info.object.enr62, info.object.enr63, info.object.enr64, info.object.enr65, info.object.enr66, info.object.enr67, info.object.enr68, info.object.enr69, info.object.enr70, info.object.enr71, info.object.enr72, info.object.enr73, info.object.enr74, info.object.enr75, info.object.enr76, info.object.enr77, info.object.enr78, info.object.enr79, info.object.enr80, info.object.enr81, info.object.enr82, info.object.enr83, info.object.enr84, info.object.enr85, info.object.enr86, info.object.enr87, info.object.enr88, info.object.enr89, info.object.enr90, info.object.enr91, info.object.enr92]
        li6vals = [info.object.Li6_1, info.object.Li6_2, info.object.Li6_3, info.object.Li6_4, info.object.Li6_5, info.object.Li6_6, info.object.Li6_7, info.object.Li6_8, info.object.Li6_9, info.object.Li6_10, info.object.Li6_11, info.object.Li6_12, info.object.Li6_13, info.object.Li6_14, info.object.Li6_15, info.object.Li6_16, info.object.Li6_17, info.object.Li6_18, info.object.Li6_19, info.object.Li6_20, info.object.Li6_21, info.object.Li6_22, info.object.Li6_23, info.object.Li6_24, info.object.Li6_25, info.object.Li6_26, info.object.Li6_27, info.object.Li6_28, info.object.Li6_29, info.object.Li6_30, info.object.Li6_31, info.object.Li6_32, info.object.Li6_33, info.object.Li6_34, info.object.Li6_35, info.object.Li6_36, info.object.Li6_37, info.object.Li6_38, info.object.Li6_39, info.object.Li6_40, info.object.Li6_41, info.object.Li6_42, info.object.Li6_43, info.object.Li6_44, info.object.Li6_45, info.object.Li6_46, info.object.Li6_47, info.object.Li6_48, info.object.Li6_49, info.object.Li6_50, info.object.Li6_51, info.object.Li6_52, info.object.Li6_53, info.object.Li6_54, info.object.Li6_55, info.object.Li6_56, info.object.Li6_57, info.object.Li6_58, info.object.Li6_59, info.object.Li6_60, info.object.Li6_61, info.object.Li6_62, info.object.Li6_63, info.object.Li6_64, info.object.Li6_65, info.object.Li6_66, info.object.Li6_67, info.object.Li6_68, info.object.Li6_69, info.object.Li6_70, info.object.Li6_71, info.object.Li6_72, info.object.Li6_73, info.object.Li6_74, info.object.Li6_75, info.object.Li6_76, info.object.Li6_77, info.object.Li6_78, info.object.Li6_79, info.object.Li6_80, info.object.Li6_81, info.object.Li6_82, info.object.Li6_83, info.object.Li6_84, info.object.Li6_85, info.object.Li6_86, info.object.Li6_87, info.object.Li6_88, info.object.Li6_89, info.object.Li6_90, info.object.Li6_91, info.object.Li6_92]
        li7vals = [info.object.Li7_1, info.object.Li7_2, info.object.Li7_3, info.object.Li7_4, info.object.Li7_5, info.object.Li7_6, info.object.Li7_7, info.object.Li7_8, info.object.Li7_9, info.object.Li7_10, info.object.Li7_11, info.object.Li7_12, info.object.Li7_13, info.object.Li7_14, info.object.Li7_15, info.object.Li7_16, info.object.Li7_17, info.object.Li7_18, info.object.Li7_19, info.object.Li7_20, info.object.Li7_21, info.object.Li7_22, info.object.Li7_23, info.object.Li7_24, info.object.Li7_25, info.object.Li7_26, info.object.Li7_27, info.object.Li7_28, info.object.Li7_29, info.object.Li7_30, info.object.Li7_31, info.object.Li7_32, info.object.Li7_33, info.object.Li7_34, info.object.Li7_35, info.object.Li7_36, info.object.Li7_37, info.object.Li7_38, info.object.Li7_39, info.object.Li7_40, info.object.Li7_41, info.object.Li7_42, info.object.Li7_43, info.object.Li7_44, info.object.Li7_45, info.object.Li7_46, info.object.Li7_47, info.object.Li7_48, info.object.Li7_49, info.object.Li7_50, info.object.Li7_51, info.object.Li7_52, info.object.Li7_53, info.object.Li7_54, info.object.Li7_55, info.object.Li7_56, info.object.Li7_57, info.object.Li7_58, info.object.Li7_59, info.object.Li7_60, info.object.Li7_61, info.object.Li7_62, info.object.Li7_63, info.object.Li7_64, info.object.Li7_65, info.object.Li7_66, info.object.Li7_67, info.object.Li7_68, info.object.Li7_69, info.object.Li7_70, info.object.Li7_71, info.object.Li7_72, info.object.Li7_73, info.object.Li7_74, info.object.Li7_75, info.object.Li7_76, info.object.Li7_77, info.object.Li7_78, info.object.Li7_79, info.object.Li7_80, info.object.Li7_81, info.object.Li7_82, info.object.Li7_83, info.object.Li7_84, info.object.Li7_85, info.object.Li7_86, info.object.Li7_87, info.object.Li7_88, info.object.Li7_89, info.object.Li7_90, info.object.Li7_91, info.object.Li7_92]
        b10vals = [info.object.B10_1, info.object.B10_2, info.object.B10_3, info.object.B10_4, info.object.B10_5, info.object.B10_6, info.object.B10_7, info.object.B10_8, info.object.B10_9, info.object.B10_10, info.object.B10_11, info.object.B10_12, info.object.B10_13, info.object.B10_14, info.object.B10_15, info.object.B10_16, info.object.B10_17, info.object.B10_18, info.object.B10_19, info.object.B10_20, info.object.B10_21, info.object.B10_22, info.object.B10_23, info.object.B10_24, info.object.B10_25, info.object.B10_26, info.object.B10_27, info.object.B10_28, info.object.B10_29, info.object.B10_30, info.object.B10_31, info.object.B10_32, info.object.B10_33, info.object.B10_34, info.object.B10_35, info.object.B10_36, info.object.B10_37, info.object.B10_38, info.object.B10_39, info.object.B10_40, info.object.B10_41, info.object.B10_42, info.object.B10_43, info.object.B10_44, info.object.B10_45, info.object.B10_46, info.object.B10_47, info.object.B10_48, info.object.B10_49, info.object.B10_50, info.object.B10_51, info.object.B10_52, info.object.B10_53, info.object.B10_54, info.object.B10_55, info.object.B10_56, info.object.B10_57, info.object.B10_58, info.object.B10_59, info.object.B10_60, info.object.B10_61, info.object.B10_62, info.object.B10_63, info.object.B10_64, info.object.B10_65, info.object.B10_66, info.object.B10_67, info.object.B10_68, info.object.B10_69, info.object.B10_70, info.object.B10_71, info.object.B10_72, info.object.B10_73, info.object.B10_74, info.object.B10_75, info.object.B10_76, info.object.B10_77, info.object.B10_78, info.object.B10_79, info.object.B10_80, info.object.B10_81, info.object.B10_82, info.object.B10_83, info.object.B10_84, info.object.B10_85, info.object.B10_86, info.object.B10_87, info.object.B10_88, info.object.B10_89, info.object.B10_90, info.object.B10_91, info.object.B10_92]
        b11vals = [info.object.B11_1, info.object.B11_2, info.object.B11_3, info.object.B11_4, info.object.B11_5, info.object.B11_6, info.object.B11_7, info.object.B11_8, info.object.B11_9, info.object.B11_10, info.object.B11_11, info.object.B11_12, info.object.B11_13, info.object.B11_14, info.object.B11_15, info.object.B11_16, info.object.B11_17, info.object.B11_18, info.object.B11_19, info.object.B11_20, info.object.B11_21, info.object.B11_22, info.object.B11_23, info.object.B11_24, info.object.B11_25, info.object.B11_26, info.object.B11_27, info.object.B11_28, info.object.B11_29, info.object.B11_30, info.object.B11_31, info.object.B11_32, info.object.B11_33, info.object.B11_34, info.object.B11_35, info.object.B11_36, info.object.B11_37, info.object.B11_38, info.object.B11_39, info.object.B11_40, info.object.B11_41, info.object.B11_42, info.object.B11_43, info.object.B11_44, info.object.B11_45, info.object.B11_46, info.object.B11_47, info.object.B11_48, info.object.B11_49, info.object.B11_50, info.object.B11_51, info.object.B11_52, info.object.B11_53, info.object.B11_54, info.object.B11_55, info.object.B11_56, info.object.B11_57, info.object.B11_58, info.object.B11_59, info.object.B11_60, info.object.B11_61, info.object.B11_62, info.object.B11_63, info.object.B11_64, info.object.B11_65, info.object.B11_66, info.object.B11_67, info.object.B11_68, info.object.B11_69, info.object.B11_70, info.object.B11_71, info.object.B11_72, info.object.B11_73, info.object.B11_74, info.object.B11_75, info.object.B11_76, info.object.B11_77, info.object.B11_78, info.object.B11_79, info.object.B11_80, info.object.B11_81, info.object.B11_82, info.object.B11_83, info.object.B11_84, info.object.B11_85, info.object.B11_86, info.object.B11_87, info.object.B11_88, info.object.B11_89, info.object.B11_90, info.object.B11_91, info.object.B11_92]
        u234vals = [info.object.U234_1, info.object.U234_2, info.object.U234_3, info.object.U234_4, info.object.U234_5, info.object.U234_6, info.object.U234_7, info.object.U234_8, info.object.U234_9, info.object.U234_10, info.object.U234_11, info.object.U234_12, info.object.U234_13, info.object.U234_14, info.object.U234_15, info.object.U234_16, info.object.U234_17, info.object.U234_18, info.object.U234_19, info.object.U234_20, info.object.U234_21, info.object.U234_22, info.object.U234_23, info.object.U234_24, info.object.U234_25, info.object.U234_26, info.object.U234_27, info.object.U234_28, info.object.U234_29, info.object.U234_30, info.object.U234_31, info.object.U234_32, info.object.U234_33, info.object.U234_34, info.object.U234_35, info.object.U234_36, info.object.U234_37, info.object.U234_38, info.object.U234_39, info.object.U234_40, info.object.U234_41, info.object.U234_42, info.object.U234_43, info.object.U234_44, info.object.U234_45, info.object.U234_46, info.object.U234_47, info.object.U234_48, info.object.U234_49, info.object.U234_50, info.object.U234_51, info.object.U234_52, info.object.U234_53, info.object.U234_54, info.object.U234_55, info.object.U234_56, info.object.U234_57, info.object.U234_58, info.object.U234_59, info.object.U234_60, info.object.U234_61, info.object.U234_62, info.object.U234_63, info.object.U234_64, info.object.U234_65, info.object.U234_66, info.object.U234_67, info.object.U234_68, info.object.U234_69, info.object.U234_70, info.object.U234_71, info.object.U234_72, info.object.U234_73, info.object.U234_74, info.object.U234_75, info.object.U234_76, info.object.U234_77, info.object.U234_78, info.object.U234_79, info.object.U234_81, info.object.U234_82, info.object.U234_83, info.object.U234_84, info.object.U234_85, info.object.U234_86, info.object.U234_87, info.object.U234_88, info.object.U234_89, info.object.U234_90, info.object.U234_91, info.object.U234_92]
        u235vals = [info.object.U235_1, info.object.U235_2, info.object.U235_3, info.object.U235_4, info.object.U235_5, info.object.U235_6, info.object.U235_7, info.object.U235_8, info.object.U235_9, info.object.U235_10, info.object.U235_11, info.object.U235_12, info.object.U235_13, info.object.U235_14, info.object.U235_15, info.object.U235_16, info.object.U235_17, info.object.U235_18, info.object.U235_19, info.object.U235_20, info.object.U235_21, info.object.U235_22, info.object.U235_23, info.object.U235_24, info.object.U235_25, info.object.U235_26, info.object.U235_27, info.object.U235_28, info.object.U235_29, info.object.U235_30, info.object.U235_31, info.object.U235_32, info.object.U235_33, info.object.U235_34, info.object.U235_35, info.object.U235_36, info.object.U235_37, info.object.U235_38, info.object.U235_39, info.object.U235_40, info.object.U235_41, info.object.U235_42, info.object.U235_43, info.object.U235_44, info.object.U235_45, info.object.U235_46, info.object.U235_47, info.object.U235_48, info.object.U235_49, info.object.U235_50, info.object.U235_51, info.object.U235_52, info.object.U235_53, info.object.U235_54, info.object.U235_55, info.object.U235_56, info.object.U235_57, info.object.U235_58, info.object.U235_59, info.object.U235_60, info.object.U235_61, info.object.U235_62, info.object.U235_63, info.object.U235_64, info.object.U235_65, info.object.U235_66, info.object.U235_67, info.object.U235_68, info.object.U235_69, info.object.U235_70, info.object.U235_71, info.object.U235_72, info.object.U235_73, info.object.U235_74, info.object.U235_75, info.object.U235_76, info.object.U235_77, info.object.U235_78, info.object.U235_79, info.object.U235_81, info.object.U235_82, info.object.U235_83, info.object.U235_84, info.object.U235_85, info.object.U235_86, info.object.U235_87, info.object.U235_88, info.object.U235_89, info.object.U235_90, info.object.U235_91, info.object.U235_92]
        u238vals = [info.object.U238_1, info.object.U238_2, info.object.U238_3, info.object.U238_4, info.object.U238_5, info.object.U238_6, info.object.U238_7, info.object.U238_8, info.object.U238_9, info.object.U238_10, info.object.U238_11, info.object.U238_12, info.object.U238_13, info.object.U238_14, info.object.U238_15, info.object.U238_16, info.object.U238_17, info.object.U238_18, info.object.U238_19, info.object.U238_20, info.object.U238_21, info.object.U238_22, info.object.U238_23, info.object.U238_24, info.object.U238_25, info.object.U238_26, info.object.U238_27, info.object.U238_28, info.object.U238_29, info.object.U238_30, info.object.U238_31, info.object.U238_32, info.object.U238_33, info.object.U238_34, info.object.U238_35, info.object.U238_36, info.object.U238_37, info.object.U238_38, info.object.U238_39, info.object.U238_40, info.object.U238_41, info.object.U238_42, info.object.U238_43, info.object.U238_44, info.object.U238_45, info.object.U238_46, info.object.U238_47, info.object.U238_48, info.object.U238_49, info.object.U238_50, info.object.U238_51, info.object.U238_52, info.object.U238_53, info.object.U238_54, info.object.U238_55, info.object.U238_56, info.object.U238_57, info.object.U238_58, info.object.U238_59, info.object.U238_60, info.object.U238_61, info.object.U238_62, info.object.U238_63, info.object.U238_64, info.object.U238_65, info.object.U238_66, info.object.U238_67, info.object.U238_68, info.object.U238_69, info.object.U238_70, info.object.U238_71, info.object.U238_72, info.object.U238_73, info.object.U238_74, info.object.U238_75, info.object.U238_76, info.object.U238_77, info.object.U238_78, info.object.U238_79, info.object.U238_81, info.object.U238_82, info.object.U238_83, info.object.U238_84, info.object.U238_85, info.object.U238_86, info.object.U238_87, info.object.U238_88, info.object.U238_89, info.object.U238_90, info.object.U238_91, info.object.U238_92]
        ############################### print the material fractions ###################################
        for i in range(info.object.num_ele):
            if enriched[i] == "Natural":
                inp_file.write('nat ')
            elif enriched[i] == "Enriched":
                inp_file.write('enr ')
            inp_file.write('%-3d' % (elements[i]))
            inp_file.write('%-10.7f\n' % (fractions[i]))
            if enriched[i] == "Enriched":
                if elements[i] == 3:
                    inp_file.write('  6    %-10.7f\n' % (li6vals[i]))
                    inp_file.write('  7    %-10.7f\n' % (li7vals[i]))
                elif elements[i] == 5:
                    inp_file.write('  10    %-10.7f\n' % (b10vals[i]))
                    inp_file.write('  11    %-10.7f\n' % (b11vals[i]))
                elif elements[i] == 92:
                    inp_file.write('  234    %-10.7f\n' % (u234vals[i]))
                    inp_file.write('  235    %-10.7f\n' % (u235vals[i]))
                    inp_file.write('  238    %-10.7f\n' % (u238vals[i]))
        ############################# print the MCNP material numnber ##################################
        inp_file.write('%-10d\n' % (info.object.mcnp_num)) 
        ############################## close the MatMCNP input file ####################################
        inp_file.close()
        ############################### run nuget on the input file ####################################
        mroot = os.environ['MAT_MCNP_ROOT']
        os.chdir(mroot)
        os.system('cp '+info.object.file_location+'/'+info.object.file_name+'.inp '+mroot)
        print('./MatMCNP '+info.object.file_name+'.inp')
        os.system('./MatMCNP '+info.object.file_name+'.inp')
        os.system('mv '+info.object.file_name+'.out '+info.object.file_location)
        os.system('rm '+info.object.file_name+'.inp')

class InputFile(HasTraits):
    file_name = Str
    file_location = Directory(editor = DirectoryEditor())
    title = Str
    comments = Str(editor = TextEditor())
    phys_dens = Float
    frac_type = Enum("Atomic", "Weight")
    mcnp_num = Int
    num_ele = Int
    ele1 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele2 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele3 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele4 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele5 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele6 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele7 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele8 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele9 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele10 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele11 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele12 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele13 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele14 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele15 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele16 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele17 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele18 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele19 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele20 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele21 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele22 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele23 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele24 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele25 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele26 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele27 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele28 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele29 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele30 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele31 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele32 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele33 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele34 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele35 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele36 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele37 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele38 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele39 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele40 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele41 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele42 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele43 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele44 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele45 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele46 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele47 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele48 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele49 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele50 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele51 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele52 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele53 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele54 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele55 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele56 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele57 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele58 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele59 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele60 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele61 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele62 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele63 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele64 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele65 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele66 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele67 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele68 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele69 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele70 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele71 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele72 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele73 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele74 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele75 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele76 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele77 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele78 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele79 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele80 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele81 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele82 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele83 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele84 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele85 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele86 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele87 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele88 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele89 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele90 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele91 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    ele92 = Enum("1-H","2-He","3-Li","4-Be","5-B","6-C","7-N","8-O","9-F","10-Ne","11-Na","12-Mg","13-Al","14-Si","15-P","16-S","17-Cl","18-Ar","19-K","20-Ca","21-Sc","22-Ti","23-V","24-Cr","25-Mn","26-Fe","27-Co","28-Ni","29-Cu","30-Zn","31-Ga","32-Ge","33-As","34-Se","35-Br","36-Kr","37-Rb","38-Sr","39-Y","40-Zr","41-Nb","42-Mo","44-Ru","45-Rh","46-Pd","47-Ag","48-Cd","49-In","50-Sn","51-Sb","52-Te","53-I","54-Xe","55-Cs","56-Ba","57-La","58-Ce","59-Pr","60-Nd","62-Sm","63-Eu","64-Gd","65-Tb","66-Dy","67-Ho","68-Er","69-Tm","70-Yb","71-Lu","72-Hf","73-Ta","74-W","75-Re","76-Os","77-Ir","78-Pt","79-Au","80-Hg","81-Tl","82-Pb","83-Bi","90-Th","92-U")
    frac1 = Float
    frac2 = Float
    frac3 = Float
    frac4 = Float
    frac5 = Float
    frac6 = Float
    frac7 = Float
    frac8 = Float
    frac9 = Float
    frac10 = Float
    frac11 = Float
    frac12 = Float
    frac13 = Float
    frac14 = Float
    frac15 = Float
    frac16 = Float
    frac17 = Float
    frac18 = Float
    frac19 = Float
    frac20 = Float
    frac21 = Float
    frac22 = Float
    frac23 = Float
    frac24 = Float
    frac25 = Float
    frac26 = Float
    frac27 = Float
    frac28 = Float
    frac29 = Float
    frac30 = Float
    frac31 = Float
    frac32 = Float
    frac33 = Float
    frac34 = Float
    frac35 = Float
    frac36 = Float
    frac37 = Float
    frac38 = Float
    frac39 = Float
    frac40 = Float
    frac41 = Float
    frac42 = Float
    frac43 = Float
    frac44 = Float
    frac45 = Float
    frac46 = Float
    frac47 = Float
    frac48 = Float
    frac49 = Float
    frac50 = Float
    frac51 = Float
    frac52 = Float
    frac53 = Float
    frac54 = Float
    frac55 = Float
    frac56 = Float
    frac57 = Float
    frac58 = Float
    frac59 = Float
    frac60 = Float
    frac61 = Float
    frac62 = Float
    frac63 = Float
    frac64 = Float
    frac65 = Float
    frac66 = Float
    frac67 = Float
    frac68 = Float
    frac69 = Float
    frac70 = Float
    frac71 = Float
    frac72 = Float
    frac73 = Float
    frac74 = Float
    frac75 = Float
    frac76 = Float
    frac77 = Float
    frac78 = Float
    frac79 = Float
    frac80 = Float
    frac81 = Float
    frac82 = Float
    frac83 = Float
    frac84 = Float
    frac85 = Float
    frac86 = Float
    frac87 = Float
    frac88 = Float
    frac89 = Float
    frac90 = Float
    frac91 = Float
    frac92 = Float
    enr1 = Enum("Natural", "Enriched")
    enr2 = Enum("Natural", "Enriched")
    enr3 = Enum("Natural", "Enriched")
    enr4 = Enum("Natural", "Enriched")
    enr5 = Enum("Natural", "Enriched")
    enr6 = Enum("Natural", "Enriched")
    enr7 = Enum("Natural", "Enriched")
    enr8 = Enum("Natural", "Enriched")
    enr9 = Enum("Natural", "Enriched")
    enr10 = Enum("Natural", "Enriched")
    enr11 = Enum("Natural", "Enriched")
    enr12 = Enum("Natural", "Enriched")
    enr13 = Enum("Natural", "Enriched")
    enr14 = Enum("Natural", "Enriched")
    enr15 = Enum("Natural", "Enriched")
    enr16 = Enum("Natural", "Enriched")
    enr17 = Enum("Natural", "Enriched")
    enr18 = Enum("Natural", "Enriched")
    enr19 = Enum("Natural", "Enriched")
    enr20 = Enum("Natural", "Enriched")
    enr21 = Enum("Natural", "Enriched")
    enr22 = Enum("Natural", "Enriched")
    enr23 = Enum("Natural", "Enriched")
    enr24 = Enum("Natural", "Enriched")
    enr25 = Enum("Natural", "Enriched")
    enr26 = Enum("Natural", "Enriched")
    enr27 = Enum("Natural", "Enriched")
    enr28 = Enum("Natural", "Enriched")
    enr29 = Enum("Natural", "Enriched")
    enr30 = Enum("Natural", "Enriched")
    enr31 = Enum("Natural", "Enriched")
    enr32 = Enum("Natural", "Enriched")
    enr33 = Enum("Natural", "Enriched")
    enr34 = Enum("Natural", "Enriched")
    enr35 = Enum("Natural", "Enriched")
    enr36 = Enum("Natural", "Enriched")
    enr37 = Enum("Natural", "Enriched")
    enr38 = Enum("Natural", "Enriched")
    enr39 = Enum("Natural", "Enriched")
    enr40 = Enum("Natural", "Enriched")
    enr41 = Enum("Natural", "Enriched")
    enr42 = Enum("Natural", "Enriched")
    enr43 = Enum("Natural", "Enriched")
    enr44 = Enum("Natural", "Enriched")
    enr45 = Enum("Natural", "Enriched")
    enr46 = Enum("Natural", "Enriched")
    enr47 = Enum("Natural", "Enriched")
    enr48 = Enum("Natural", "Enriched")
    enr49 = Enum("Natural", "Enriched")
    enr50 = Enum("Natural", "Enriched")
    enr51 = Enum("Natural", "Enriched")
    enr52 = Enum("Natural", "Enriched")
    enr53 = Enum("Natural", "Enriched")
    enr54 = Enum("Natural", "Enriched")
    enr55 = Enum("Natural", "Enriched")
    enr56 = Enum("Natural", "Enriched")
    enr57 = Enum("Natural", "Enriched")
    enr58 = Enum("Natural", "Enriched")
    enr59 = Enum("Natural", "Enriched")
    enr60 = Enum("Natural", "Enriched")
    enr61 = Enum("Natural", "Enriched")
    enr62 = Enum("Natural", "Enriched")
    enr63 = Enum("Natural", "Enriched")
    enr64 = Enum("Natural", "Enriched")
    enr65 = Enum("Natural", "Enriched")
    enr66 = Enum("Natural", "Enriched")
    enr67 = Enum("Natural", "Enriched")
    enr68 = Enum("Natural", "Enriched")
    enr69 = Enum("Natural", "Enriched")
    enr70 = Enum("Natural", "Enriched")
    enr71 = Enum("Natural", "Enriched")
    enr72 = Enum("Natural", "Enriched")
    enr73 = Enum("Natural", "Enriched")
    enr74 = Enum("Natural", "Enriched")
    enr75 = Enum("Natural", "Enriched")
    enr76 = Enum("Natural", "Enriched")
    enr77 = Enum("Natural", "Enriched")
    enr78 = Enum("Natural", "Enriched")
    enr79 = Enum("Natural", "Enriched")
    enr80 = Enum("Natural", "Enriched")
    enr81 = Enum("Natural", "Enriched")
    enr82 = Enum("Natural", "Enriched")
    enr83 = Enum("Natural", "Enriched")
    enr84 = Enum("Natural", "Enriched")
    enr85 = Enum("Natural", "Enriched")
    enr86 = Enum("Natural", "Enriched")
    enr87 = Enum("Natural", "Enriched")
    enr88 = Enum("Natural", "Enriched")
    enr89 = Enum("Natural", "Enriched")
    enr90 = Enum("Natural", "Enriched")
    enr91 = Enum("Natural", "Enriched")
    enr92 = Enum("Natural", "Enriched")
    Li6_1 = Float
    Li6_2 = Float
    Li6_3 = Float
    Li6_4 = Float
    Li6_5 = Float
    Li6_6 = Float
    Li6_7 = Float
    Li6_8 = Float
    Li6_9 = Float
    Li6_10 = Float
    Li6_11 = Float
    Li6_12 = Float
    Li6_13 = Float
    Li6_14 = Float
    Li6_15 = Float
    Li6_16 = Float
    Li6_17 = Float
    Li6_18 = Float
    Li6_19 = Float
    Li6_20 = Float
    Li6_21 = Float
    Li6_22 = Float
    Li6_23 = Float
    Li6_24 = Float
    Li6_25 = Float
    Li6_26 = Float
    Li6_27 = Float
    Li6_28 = Float
    Li6_29 = Float
    Li6_30 = Float
    Li6_31 = Float
    Li6_32 = Float
    Li6_33 = Float
    Li6_34 = Float
    Li6_35 = Float
    Li6_36 = Float
    Li6_37 = Float
    Li6_38 = Float
    Li6_39 = Float
    Li6_40 = Float
    Li6_41 = Float
    Li6_42 = Float
    Li6_43 = Float
    Li6_44 = Float
    Li6_45 = Float
    Li6_46 = Float
    Li6_47 = Float
    Li6_48 = Float
    Li6_49 = Float
    Li6_50 = Float
    Li6_51 = Float
    Li6_52 = Float
    Li6_53 = Float
    Li6_54 = Float
    Li6_55 = Float
    Li6_56 = Float
    Li6_57 = Float
    Li6_58 = Float
    Li6_59 = Float
    Li6_60 = Float
    Li6_61 = Float
    Li6_62 = Float
    Li6_63 = Float
    Li6_64 = Float
    Li6_65 = Float
    Li6_66 = Float
    Li6_67 = Float
    Li6_68 = Float
    Li6_69 = Float
    Li6_70 = Float
    Li6_71 = Float
    Li6_72 = Float
    Li6_73 = Float
    Li6_74 = Float
    Li6_75 = Float
    Li6_76 = Float
    Li6_77 = Float
    Li6_78 = Float
    Li6_79 = Float
    Li6_80 = Float
    Li6_81 = Float
    Li6_82 = Float
    Li6_83 = Float
    Li6_84 = Float
    Li6_85 = Float
    Li6_86 = Float
    Li6_87 = Float
    Li6_88 = Float
    Li6_89 = Float
    Li6_90 = Float
    Li6_91 = Float
    Li6_92 = Float
    Li7_1 = Float
    Li7_2 = Float
    Li7_3 = Float
    Li7_4 = Float
    Li7_5 = Float
    Li7_6 = Float
    Li7_7 = Float
    Li7_8 = Float
    Li7_9 = Float
    Li7_10 = Float
    Li7_11 = Float
    Li7_12 = Float
    Li7_13 = Float
    Li7_14 = Float
    Li7_15 = Float
    Li7_16 = Float
    Li7_17 = Float
    Li7_18 = Float
    Li7_19 = Float
    Li7_20 = Float
    Li7_21 = Float
    Li7_22 = Float
    Li7_23 = Float
    Li7_24 = Float
    Li7_25 = Float
    Li7_26 = Float
    Li7_27 = Float
    Li7_28 = Float
    Li7_29 = Float
    Li7_30 = Float
    Li7_31 = Float
    Li7_32 = Float
    Li7_33 = Float
    Li7_34 = Float
    Li7_35 = Float
    Li7_36 = Float
    Li7_37 = Float
    Li7_38 = Float
    Li7_39 = Float
    Li7_40 = Float
    Li7_41 = Float
    Li7_42 = Float
    Li7_43 = Float
    Li7_44 = Float
    Li7_45 = Float
    Li7_46 = Float
    Li7_47 = Float
    Li7_48 = Float
    Li7_49 = Float
    Li7_50 = Float
    Li7_51 = Float
    Li7_52 = Float
    Li7_53 = Float
    Li7_54 = Float
    Li7_55 = Float
    Li7_56 = Float
    Li7_57 = Float
    Li7_58 = Float
    Li7_59 = Float
    Li7_60 = Float
    Li7_61 = Float
    Li7_62 = Float
    Li7_63 = Float
    Li7_64 = Float
    Li7_65 = Float
    Li7_66 = Float
    Li7_67 = Float
    Li7_68 = Float
    Li7_69 = Float
    Li7_70 = Float
    Li7_71 = Float
    Li7_72 = Float
    Li7_73 = Float
    Li7_74 = Float
    Li7_75 = Float
    Li7_76 = Float
    Li7_77 = Float
    Li7_78 = Float
    Li7_79 = Float
    Li7_80 = Float
    Li7_81 = Float
    Li7_82 = Float
    Li7_83 = Float
    Li7_84 = Float
    Li7_85 = Float
    Li7_86 = Float
    Li7_87 = Float
    Li7_88 = Float
    Li7_89 = Float
    Li7_90 = Float
    Li7_91 = Float
    Li7_92 = Float
    B10_1 = Float
    B10_2 = Float
    B10_3 = Float
    B10_4 = Float
    B10_5 = Float
    B10_6 = Float
    B10_7 = Float
    B10_8 = Float
    B10_9 = Float
    B10_10 = Float
    B10_11 = Float
    B10_12 = Float
    B10_13 = Float
    B10_14 = Float
    B10_15 = Float
    B10_16 = Float
    B10_17 = Float
    B10_18 = Float
    B10_19 = Float
    B10_20 = Float
    B10_21 = Float
    B10_22 = Float
    B10_23 = Float
    B10_24 = Float
    B10_25 = Float
    B10_26 = Float
    B10_27 = Float
    B10_28 = Float
    B10_29 = Float
    B10_30 = Float
    B10_31 = Float
    B10_32 = Float
    B10_33 = Float
    B10_34 = Float
    B10_35 = Float
    B10_36 = Float
    B10_37 = Float
    B10_38 = Float
    B10_39 = Float
    B10_40 = Float
    B10_41 = Float
    B10_42 = Float
    B10_43 = Float
    B10_44 = Float
    B10_45 = Float
    B10_46 = Float
    B10_47 = Float
    B10_48 = Float
    B10_48 = Float
    B10_49 = Float
    B10_50 = Float
    B10_51 = Float
    B10_52 = Float
    B10_53 = Float
    B10_54 = Float
    B10_55 = Float
    B10_56 = Float
    B10_57 = Float
    B10_58 = Float
    B10_59 = Float
    B10_60 = Float
    B10_61 = Float
    B10_62 = Float
    B10_63 = Float
    B10_64 = Float
    B10_65 = Float
    B10_66 = Float
    B10_67 = Float
    B10_68 = Float
    B10_69 = Float
    B10_70 = Float
    B10_71 = Float
    B10_72 = Float
    B10_73 = Float
    B10_74 = Float
    B10_75 = Float
    B10_76 = Float
    B10_77 = Float
    B10_78 = Float
    B10_79 = Float
    B10_80 = Float
    B10_81 = Float
    B10_82 = Float
    B10_83 = Float
    B10_84 = Float
    B10_85 = Float
    B10_86 = Float
    B10_87 = Float
    B10_88 = Float
    B10_89 = Float
    B10_90 = Float
    B10_91 = Float
    B10_92 = Float
    B11_1 = Float
    B11_2 = Float
    B11_3 = Float
    B11_4 = Float
    B11_5 = Float
    B11_6 = Float
    B11_7 = Float
    B11_8 = Float
    B11_9 = Float
    B11_10 = Float
    B11_11 = Float
    B11_12 = Float
    B11_13 = Float
    B11_14 = Float
    B11_15 = Float
    B11_16 = Float
    B11_17 = Float
    B11_18 = Float
    B11_19 = Float
    B11_20 = Float
    B11_21 = Float
    B11_22 = Float
    B11_23 = Float
    B11_24 = Float
    B11_25 = Float
    B11_26 = Float
    B11_27 = Float
    B11_28 = Float
    B11_29 = Float
    B11_30 = Float
    B11_31 = Float
    B11_32 = Float
    B11_33 = Float
    B11_34 = Float
    B11_35 = Float
    B11_36 = Float
    B11_37 = Float
    B11_38 = Float
    B11_39 = Float
    B11_40 = Float
    B11_41 = Float
    B11_42 = Float
    B11_43 = Float
    B11_44 = Float
    B11_45 = Float
    B11_46 = Float
    B11_47 = Float
    B11_48 = Float
    B11_48 = Float
    B11_49 = Float
    B11_50 = Float
    B11_51 = Float
    B11_52 = Float
    B11_53 = Float
    B11_54 = Float
    B11_55 = Float
    B11_56 = Float
    B11_57 = Float
    B11_58 = Float
    B11_59 = Float
    B11_60 = Float
    B11_61 = Float
    B11_62 = Float
    B11_63 = Float
    B11_64 = Float
    B11_65 = Float
    B11_66 = Float
    B11_67 = Float
    B11_68 = Float
    B11_69 = Float
    B11_70 = Float
    B11_71 = Float
    B11_72 = Float
    B11_73 = Float
    B11_74 = Float
    B11_75 = Float
    B11_76 = Float
    B11_77 = Float
    B11_78 = Float
    B11_79 = Float
    B11_80 = Float
    B11_81 = Float
    B11_82 = Float
    B11_83 = Float
    B11_84 = Float
    B11_85 = Float
    B11_86 = Float
    B11_87 = Float
    B11_88 = Float
    B11_89 = Float
    B11_90 = Float
    B11_91 = Float
    B11_92 = Float
    U234_1 = Float
    U234_2 = Float
    U234_3 = Float
    U234_4 = Float
    U234_5 = Float
    U234_6 = Float
    U234_7 = Float
    U234_8 = Float
    U234_9 = Float
    U234_10 = Float
    U234_11 = Float
    U234_12 = Float
    U234_13 = Float
    U234_14 = Float
    U234_15 = Float
    U234_16 = Float
    U234_17 = Float
    U234_18 = Float
    U234_19 = Float
    U234_20 = Float
    U234_21 = Float
    U234_22 = Float
    U234_23 = Float
    U234_24 = Float
    U234_25 = Float
    U234_26 = Float
    U234_27 = Float
    U234_28 = Float
    U234_29 = Float
    U234_30 = Float
    U234_31 = Float
    U234_32 = Float
    U234_33 = Float
    U234_34 = Float
    U234_35 = Float
    U234_36 = Float
    U234_37 = Float
    U234_38 = Float
    U234_39 = Float
    U234_40 = Float
    U234_41 = Float
    U234_42 = Float
    U234_43 = Float
    U234_44 = Float
    U234_45 = Float
    U234_46 = Float
    U234_47 = Float
    U234_48 = Float
    U234_49 = Float
    U234_50 = Float
    U234_51 = Float
    U234_52 = Float
    U234_53 = Float
    U234_54 = Float
    U234_55 = Float
    U234_56 = Float
    U234_57 = Float
    U234_58 = Float
    U234_59 = Float
    U234_60 = Float
    U234_61 = Float
    U234_62 = Float
    U234_63 = Float
    U234_64 = Float
    U234_65 = Float
    U234_66 = Float
    U234_67 = Float
    U234_68 = Float
    U234_69 = Float
    U234_70 = Float
    U234_71 = Float
    U234_72 = Float
    U234_73 = Float
    U234_74 = Float
    U234_75 = Float
    U234_76 = Float
    U234_77 = Float
    U234_78 = Float
    U234_79 = Float
    U234_80 = Float
    U234_81 = Float
    U234_82 = Float
    U234_83 = Float
    U234_84 = Float
    U234_85 = Float
    U234_86 = Float
    U234_87 = Float
    U234_88 = Float
    U234_89 = Float
    U234_90 = Float
    U234_91 = Float
    U234_92 = Float
    U235_1 = Float
    U235_2 = Float
    U235_3 = Float
    U235_4 = Float
    U235_5 = Float
    U235_6 = Float
    U235_7 = Float
    U235_8 = Float
    U235_9 = Float
    U235_10 = Float
    U235_11 = Float
    U235_12 = Float
    U235_13 = Float
    U235_14 = Float
    U235_15 = Float
    U235_16 = Float
    U235_17 = Float
    U235_18 = Float
    U235_19 = Float
    U235_20 = Float
    U235_21 = Float
    U235_22 = Float
    U235_23 = Float
    U235_24 = Float
    U235_25 = Float
    U235_26 = Float
    U235_27 = Float
    U235_28 = Float
    U235_29 = Float
    U235_30 = Float
    U235_31 = Float
    U235_32 = Float
    U235_33 = Float
    U235_34 = Float
    U235_35 = Float
    U235_36 = Float
    U235_37 = Float
    U235_38 = Float
    U235_39 = Float
    U235_40 = Float
    U235_41 = Float
    U235_42 = Float
    U235_43 = Float
    U235_44 = Float
    U235_45 = Float
    U235_46 = Float
    U235_47 = Float
    U235_48 = Float
    U235_49 = Float
    U235_50 = Float
    U235_51 = Float
    U235_52 = Float
    U235_53 = Float
    U235_54 = Float
    U235_55 = Float
    U235_56 = Float
    U235_57 = Float
    U235_58 = Float
    U235_59 = Float
    U235_60 = Float
    U235_61 = Float
    U235_62 = Float
    U235_63 = Float
    U235_64 = Float
    U235_65 = Float
    U235_66 = Float
    U235_67 = Float
    U235_68 = Float
    U235_69 = Float
    U235_70 = Float
    U235_71 = Float
    U235_72 = Float
    U235_73 = Float
    U235_74 = Float
    U235_75 = Float
    U235_76 = Float
    U235_77 = Float
    U235_78 = Float
    U235_79 = Float
    U235_80 = Float
    U235_81 = Float
    U235_82 = Float
    U235_83 = Float
    U235_84 = Float
    U235_85 = Float
    U235_86 = Float
    U235_87 = Float
    U235_88 = Float
    U235_89 = Float
    U235_90 = Float
    U235_91 = Float
    U235_92 = Float
    U238_1 = Float
    U238_2 = Float
    U238_3 = Float
    U238_4 = Float
    U238_5 = Float
    U238_6 = Float
    U238_7 = Float
    U238_8 = Float
    U238_9 = Float
    U238_10 = Float
    U238_11 = Float
    U238_12 = Float
    U238_13 = Float
    U238_14 = Float
    U238_15 = Float
    U238_16 = Float
    U238_17 = Float
    U238_18 = Float
    U238_19 = Float
    U238_20 = Float
    U238_21 = Float
    U238_22 = Float
    U238_23 = Float
    U238_24 = Float
    U238_25 = Float
    U238_26 = Float
    U238_27 = Float
    U238_28 = Float
    U238_29 = Float
    U238_30 = Float
    U238_31 = Float
    U238_32 = Float
    U238_33 = Float
    U238_34 = Float
    U238_35 = Float
    U238_36 = Float
    U238_37 = Float
    U238_38 = Float
    U238_39 = Float
    U238_40 = Float
    U238_41 = Float
    U238_42 = Float
    U238_43 = Float
    U238_44 = Float
    U238_45 = Float
    U238_46 = Float
    U238_47 = Float
    U238_48 = Float
    U238_49 = Float
    U238_50 = Float
    U238_51 = Float
    U238_52 = Float
    U238_53 = Float
    U238_54 = Float
    U238_55 = Float
    U238_56 = Float
    U238_57 = Float
    U238_58 = Float
    U238_59 = Float
    U238_60 = Float
    U238_61 = Float
    U238_62 = Float
    U238_63 = Float
    U238_64 = Float
    U238_65 = Float
    U238_66 = Float
    U238_67 = Float
    U238_68 = Float
    U238_69 = Float
    U238_70 = Float
    U238_71 = Float
    U238_72 = Float
    U238_73 = Float
    U238_74 = Float
    U238_75 = Float
    U238_76 = Float
    U238_77 = Float
    U238_78 = Float
    U238_79 = Float
    U238_80 = Float
    U238_81 = Float
    U238_82 = Float
    U238_83 = Float
    U238_84 = Float
    U238_85 = Float
    U238_86 = Float
    U238_87 = Float
    U238_88 = Float
    U238_89 = Float
    U238_90 = Float
    U238_91 = Float
    U238_92 = Float
    save_file = Button()
    run_matmcnp = Button()

view1 = View(
              Group(
                     Group(
                            Item(name = 'file_name', label = 'Input file name',
                                 tooltip = "The current state of all fields in this interface will be saved as "\
                                           "<Input file name>.pkl by clicking the \"Save file\" button on the \"Save/Run\" tab."),
                            Item(name = 'file_location', label = 'Input file location',
                                 tooltip = "This is the directory where the .pkl file will be saved."),
                            Item(name = 'title', label = 'Title',
                                 tooltip = "This is the title of the compound or mixture."),
                            Item(name = 'comments', style = 'custom', height = -400,
                                 tooltip = "This is where any comments should be placed. These comments do not "\
                                           "influence the calculation in any way. Each line of comments will be truncated after 72 characters."),
                            Item(name = 'phys_dens', label = 'Physical density (g/cc)',
                                 tooltip = "This is the physical density of the mixture in grams per cubic centimeter."),
                            Item(name = 'frac_type', label = 'Fraction type',
                                 tooltip = "This is how the fractions on the \"Material Description\" tab will be interpreted."),
                            Item(name = 'mcnp_num', label = 'MCNP material number',
                                 tooltip = "This is the MCNP material number. It has a maximum value of 99999 and will be truncated if above this value."),
                            spring,
                            orientation = 'vertical', label = 'Setup'
                          ),
                     Group(
                            Item(name = 'num_ele', label = 'Number of entries',
                                 tooltip = "This is the number of entries included in the material description. The maximum value is 92."),
                                 Group(
                                        Group(
                                               Item(name = 'ele1', label = 'Element', width = -100),
                                               Item(name = 'frac1', label = 'Fraction'),
                                               Item(name = 'enr1', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele1 == "3-Li" or ele1 == "5-B" or ele1 == "92-U"'),
                                               Item(name = 'Li6_1', label = 'Li-6 fraction',
                                                    visible_when = 'ele1 == "3-Li" and enr1 == "Enriched"'),
                                               Item(name = 'Li7_1', label = 'Li-7 fraction',
                                                    visible_when = 'ele1 == "3-Li" and enr1 == "Enriched"'),
                                               Item(name = 'B10_1', label = 'B-10 fraction',
                                                    visible_when = 'ele1 == "5-B" and enr1 == "Enriched"'),
                                               Item(name = 'B11_1', label = 'B-11 fraction',
                                                    visible_when = 'ele1 == "5-B" and enr1 == "Enriched"'),
                                               Item(name = 'U234_1', label = 'U-234 fraction',
                                                    visible_when = 'ele1 == "92-U" and enr1 == "Enriched"'),
                                               Item(name = 'U235_1', label = 'U-235 fraction',
                                                    visible_when = 'ele1 == "92-U" and enr1 == "Enriched"'),
                                               Item(name = 'U238_1', label = 'U-238 fraction',
                                                    visible_when = 'ele1 == "92-U" and enr1 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 0'
                                             ),
                                        Group(
                                               Item(name = 'ele2', label = 'Element', width = -100),
                                               Item(name = 'frac2', label = 'Fraction'),
                                               Item(name = 'enr2', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele2 == "3-Li" or ele2 == "5-B" or ele2 == "92-U"'),
                                               Item(name = 'Li6_2', label = 'Li-6 fraction',
                                                    visible_when = 'ele2 == "3-Li" and enr2 == "Enriched"'),
                                               Item(name = 'Li7_2', label = 'Li-7 fraction',
                                                    visible_when = 'ele2 == "3-Li" and enr2 == "Enriched"'),
                                               Item(name = 'B10_2', label = 'B-10 fraction',
                                                    visible_when = 'ele2 == "5-B" and enr2 == "Enriched"'),
                                               Item(name = 'B11_2', label = 'B-11 fraction',
                                                    visible_when = 'ele2 == "5-B" and enr2 == "Enriched"'),
                                               Item(name = 'U234_2', label = 'U-234 fraction',
                                                    visible_when = 'ele2 == "92-U" and enr2 == "Enriched"'),
                                               Item(name = 'U235_2', label = 'U-235 fraction',
                                                    visible_when = 'ele2 == "92-U" and enr2 == "Enriched"'),
                                               Item(name = 'U238_2', label = 'U-238 fraction',
                                                    visible_when = 'ele2 == "92-U" and enr2 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 1'
                                             ),
                                        Group(
                                               Item(name = 'ele3', label = 'Element', width = -100),
                                               Item(name = 'frac3', label = 'Fraction'),
                                               Item(name = 'enr3', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele3 == "3-Li" or ele3 == "5-B" or ele3 == "92-U"'),
                                               Item(name = 'Li6_3', label = 'Li-6 fraction',
                                                    visible_when = 'ele3 == "3-Li" and enr3 == "Enriched"'),
                                               Item(name = 'Li7_3', label = 'Li-7 fraction',
                                                    visible_when = 'ele3 == "3-Li" and enr3 == "Enriched"'),
                                               Item(name = 'B10_3', label = 'B-10 fraction',
                                                    visible_when = 'ele3 == "5-B" and enr3 == "Enriched"'),
                                               Item(name = 'B11_3', label = 'B-11 fraction',
                                                    visible_when = 'ele3 == "5-B" and enr3 == "Enriched"'),
                                               Item(name = 'U234_3', label = 'U-234 fraction',
                                                    visible_when = 'ele3 == "92-U" and enr3 == "Enriched"'),
                                               Item(name = 'U235_3', label = 'U-235 fraction',
                                                    visible_when = 'ele3 == "92-U" and enr3 == "Enriched"'),
                                               Item(name = 'U238_3', label = 'U-238 fraction',
                                                    visible_when = 'ele3 == "92-U" and enr3 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 2'
                                             ),
                                        Group(
                                               Item(name = 'ele4', label = 'Element', width = -100),
                                               Item(name = 'frac4', label = 'Fraction'),
                                               Item(name = 'enr4', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele4 == "3-Li" or ele4 == "5-B" or ele4 == "92-U"'),
                                               Item(name = 'Li6_4', label = 'Li-6 fraction',
                                                    visible_when = 'ele4 == "3-Li" and enr4 == "Enriched"'),
                                               Item(name = 'Li7_4', label = 'Li-7 fraction',
                                                    visible_when = 'ele4 == "3-Li" and enr4 == "Enriched"'),
                                               Item(name = 'B10_4', label = 'B-10 fraction',
                                                    visible_when = 'ele4 == "5-B" and enr4 == "Enriched"'),
                                               Item(name = 'B11_4', label = 'B-11 fraction',
                                                    visible_when = 'ele4 == "5-B" and enr4 == "Enriched"'),
                                               Item(name = 'U234_4', label = 'U-234 fraction',
                                                    visible_when = 'ele4 == "92-U" and enr4 == "Enriched"'),
                                               Item(name = 'U235_4', label = 'U-235 fraction',
                                                    visible_when = 'ele4 == "92-U" and enr4 == "Enriched"'),
                                               Item(name = 'U238_4', label = 'U-238 fraction',
                                                    visible_when = 'ele4 == "92-U" and enr4 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 3'
                                             ),
                                        Group(
                                               Item(name = 'ele5', label = 'Element', width = -100),
                                               Item(name = 'frac5', label = 'Fraction'),
                                               Item(name = 'enr5', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele5 == "3-Li" or ele5 == "5-B" or ele5 == "92-U"'),
                                               Item(name = 'Li6_5', label = 'Li-6 fraction',
                                                    visible_when = 'ele5 == "3-Li" and enr5 == "Enriched"'),
                                               Item(name = 'Li7_5', label = 'Li-7 fraction',
                                                    visible_when = 'ele5 == "3-Li" and enr5 == "Enriched"'),
                                               Item(name = 'B10_5', label = 'B-10 fraction',
                                                    visible_when = 'ele5 == "5-B" and enr5 == "Enriched"'),
                                               Item(name = 'B11_5', label = 'B-11 fraction',
                                                    visible_when = 'ele5 == "5-B" and enr5 == "Enriched"'),
                                               Item(name = 'U234_5', label = 'U-234 fraction',
                                                    visible_when = 'ele5 == "92-U" and enr5 == "Enriched"'),
                                               Item(name = 'U235_5', label = 'U-235 fraction',
                                                    visible_when = 'ele5 == "92-U" and enr5 == "Enriched"'),
                                               Item(name = 'U238_5', label = 'U-238 fraction',
                                                    visible_when = 'ele5 == "92-U" and enr5 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 4'
                                             ),
                                        Group(
                                               Item(name = 'ele6', label = 'Element', width = -100),
                                               Item(name = 'frac6', label = 'Fraction'),
                                               Item(name = 'enr6', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele6 == "3-Li" or ele6 == "5-B" or ele6 == "92-U"'),
                                               Item(name = 'Li6_6', label = 'Li-6 fraction',
                                                    visible_when = 'ele6 == "3-Li" and enr6 == "Enriched"'),
                                               Item(name = 'Li7_6', label = 'Li-7 fraction',
                                                    visible_when = 'ele6 == "3-Li" and enr6 == "Enriched"'),
                                               Item(name = 'B10_6', label = 'B-10 fraction',
                                                    visible_when = 'ele6 == "5-B" and enr6 == "Enriched"'),
                                               Item(name = 'B11_6', label = 'B-11 fraction',
                                                    visible_when = 'ele6 == "5-B" and enr6 == "Enriched"'),
                                               Item(name = 'U234_6', label = 'U-234 fraction',
                                                    visible_when = 'ele6 == "92-U" and enr6 == "Enriched"'),
                                               Item(name = 'U235_6', label = 'U-235 fraction',
                                                    visible_when = 'ele6 == "92-U" and enr6 == "Enriched"'),
                                               Item(name = 'U238_6', label = 'U-238 fraction',
                                                    visible_when = 'ele6 == "92-U" and enr6 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 5'
                                             ),
                                        Group(
                                               Item(name = 'ele7', label = 'Element', width = -100),
                                               Item(name = 'frac7', label = 'Fraction'),
                                               Item(name = 'enr7', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele7 == "3-Li" or ele7 == "5-B" or ele7 == "92-U"'),
                                               Item(name = 'Li6_7', label = 'Li-6 fraction',
                                                    visible_when = 'ele7 == "3-Li" and enr7 == "Enriched"'),
                                               Item(name = 'Li7_7', label = 'Li-7 fraction',
                                                    visible_when = 'ele7 == "3-Li" and enr7 == "Enriched"'),
                                               Item(name = 'B10_7', label = 'B-10 fraction',
                                                    visible_when = 'ele7 == "5-B" and enr7 == "Enriched"'),
                                               Item(name = 'B11_7', label = 'B-11 fraction',
                                                    visible_when = 'ele7 == "5-B" and enr7 == "Enriched"'),
                                               Item(name = 'U234_7', label = 'U-234 fraction',
                                                    visible_when = 'ele7 == "92-U" and enr7 == "Enriched"'),
                                               Item(name = 'U235_7', label = 'U-235 fraction',
                                                    visible_when = 'ele7 == "92-U" and enr7 == "Enriched"'),
                                               Item(name = 'U238_7', label = 'U-238 fraction',
                                                    visible_when = 'ele7 == "92-U" and enr7 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 6'
                                             ),
                                        Group(
                                               Item(name = 'ele8', label = 'Element', width = -100),
                                               Item(name = 'frac8', label = 'Fraction'),
                                               Item(name = 'enr8', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele8 == "3-Li" or ele8 == "5-B" or ele8 == "92-U"'),
                                               Item(name = 'Li6_8', label = 'Li-6 fraction',
                                                    visible_when = 'ele8 == "3-Li" and enr8 == "Enriched"'),
                                               Item(name = 'Li7_8', label = 'Li-7 fraction',
                                                    visible_when = 'ele8 == "3-Li" and enr8 == "Enriched"'),
                                               Item(name = 'B10_8', label = 'B-10 fraction',
                                                    visible_when = 'ele8 == "5-B" and enr8 == "Enriched"'),
                                               Item(name = 'B11_8', label = 'B-11 fraction',
                                                    visible_when = 'ele8 == "5-B" and enr8 == "Enriched"'),
                                               Item(name = 'U234_8', label = 'U-234 fraction',
                                                    visible_when = 'ele8 == "92-U" and enr8 == "Enriched"'),
                                               Item(name = 'U235_8', label = 'U-235 fraction',
                                                    visible_when = 'ele8 == "92-U" and enr8 == "Enriched"'),
                                               Item(name = 'U238_8', label = 'U-238 fraction',
                                                    visible_when = 'ele8 == "92-U" and enr8 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 7'
                                             ),
                                        Group(
                                               Item(name = 'ele9', label = 'Element', width = -100),
                                               Item(name = 'frac9', label = 'Fraction'),
                                               Item(name = 'enr9', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele9 == "3-Li" or ele9 == "5-B" or ele9 == "92-U"'),
                                               Item(name = 'Li6_9', label = 'Li-6 fraction',
                                                    visible_when = 'ele9 == "3-Li" and enr9 == "Enriched"'),
                                               Item(name = 'Li7_9', label = 'Li-7 fraction',
                                                    visible_when = 'ele9 == "3-Li" and enr9 == "Enriched"'),
                                               Item(name = 'B10_9', label = 'B-10 fraction',
                                                    visible_when = 'ele9 == "5-B" and enr9 == "Enriched"'),
                                               Item(name = 'B11_9', label = 'B-11 fraction',
                                                    visible_when = 'ele9 == "5-B" and enr9 == "Enriched"'),
                                               Item(name = 'U234_9', label = 'U-234 fraction',
                                                    visible_when = 'ele9 == "92-U" and enr9 == "Enriched"'),
                                               Item(name = 'U235_9', label = 'U-235 fraction',
                                                    visible_when = 'ele9 == "92-U" and enr9 == "Enriched"'),
                                               Item(name = 'U238_9', label = 'U-238 fraction',
                                                    visible_when = 'ele9 == "92-U" and enr9 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 8'
                                             ),
                                        Group(
                                               Item(name = 'ele10', label = 'Element', width = -100),
                                               Item(name = 'frac10', label = 'Fraction'),
                                               Item(name = 'enr10', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele10 == "3-Li" or ele10 == "5-B" or ele10 == "92-U"'),
                                               Item(name = 'Li6_10', label = 'Li-6 fraction',
                                                    visible_when = 'ele10 == "3-Li" and enr10 == "Enriched"'),
                                               Item(name = 'Li7_10', label = 'Li-7 fraction',
                                                    visible_when = 'ele10 == "3-Li" and enr10 == "Enriched"'),
                                               Item(name = 'B10_10', label = 'B-10 fraction',
                                                    visible_when = 'ele10 == "5-B" and enr10 == "Enriched"'),
                                               Item(name = 'B11_10', label = 'B-11 fraction',
                                                    visible_when = 'ele10 == "5-B" and enr10 == "Enriched"'),
                                               Item(name = 'U234_10', label = 'U-234 fraction',
                                                    visible_when = 'ele10 == "92-U" and enr10 == "Enriched"'),
                                               Item(name = 'U235_10', label = 'U-235 fraction',
                                                    visible_when = 'ele10 == "92-U" and enr10 == "Enriched"'),
                                               Item(name = 'U238_10', label = 'U-238 fraction',
                                                    visible_when = 'ele10 == "92-U" and enr10 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 9'
                                             ),
                                        Group(
                                               Item(name = 'ele11', label = 'Element', width = -100),
                                               Item(name = 'frac11', label = 'Fraction'),
                                               Item(name = 'enr11', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele11 == "3-Li" or ele11 == "5-B" or ele11 == "92-U"'),
                                               Item(name = 'Li6_11', label = 'Li-6 fraction',
                                                    visible_when = 'ele11 == "3-Li" and enr11 == "Enriched"'),
                                               Item(name = 'Li7_11', label = 'Li-7 fraction',
                                                    visible_when = 'ele11 == "3-Li" and enr11 == "Enriched"'),
                                               Item(name = 'B10_11', label = 'B-10 fraction',
                                                    visible_when = 'ele11 == "5-B" and enr11 == "Enriched"'),
                                               Item(name = 'B11_11', label = 'B-11 fraction',
                                                    visible_when = 'ele11 == "5-B" and enr11 == "Enriched"'),
                                               Item(name = 'U234_11', label = 'U-234 fraction',
                                                    visible_when = 'ele11 == "92-U" and enr11 == "Enriched"'),
                                               Item(name = 'U235_11', label = 'U-235 fraction',
                                                    visible_when = 'ele11 == "92-U" and enr11 == "Enriched"'),
                                               Item(name = 'U238_11', label = 'U-238 fraction',
                                                    visible_when = 'ele11 == "92-U" and enr11 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 10'
                                             ),
                                        Group(
                                               Item(name = 'ele12', label = 'Element', width = -100),
                                               Item(name = 'frac12', label = 'Fraction'),
                                               Item(name = 'enr12', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele12 == "3-Li" or ele12 == "5-B" or ele12 == "92-U"'),
                                               Item(name = 'Li6_12', label = 'Li-6 fraction',
                                                    visible_when = 'ele12 == "3-Li" and enr12 == "Enriched"'),
                                               Item(name = 'Li7_12', label = 'Li-7 fraction',
                                                    visible_when = 'ele12 == "3-Li" and enr12 == "Enriched"'),
                                               Item(name = 'B10_12', label = 'B-10 fraction',
                                                    visible_when = 'ele12 == "5-B" and enr12 == "Enriched"'),
                                               Item(name = 'B11_12', label = 'B-11 fraction',
                                                    visible_when = 'ele12 == "5-B" and enr12 == "Enriched"'),
                                               Item(name = 'U234_12', label = 'U-234 fraction',
                                                    visible_when = 'ele12 == "92-U" and enr12 == "Enriched"'),
                                               Item(name = 'U235_12', label = 'U-235 fraction',
                                                    visible_when = 'ele12 == "92-U" and enr12 == "Enriched"'),
                                               Item(name = 'U238_12', label = 'U-238 fraction',
                                                    visible_when = 'ele12 == "92-U" and enr12 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 11'
                                             ),
                                        Group(
                                               Item(name = 'ele13', label = 'Element', width = -100),
                                               Item(name = 'frac13', label = 'Fraction'),
                                               Item(name = 'enr13', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele13 == "3-Li" or ele13 == "5-B" or ele13 == "92-U"'),
                                               Item(name = 'Li6_13', label = 'Li-6 fraction',
                                                    visible_when = 'ele13 == "3-Li" and enr13 == "Enriched"'),
                                               Item(name = 'Li7_13', label = 'Li-7 fraction',
                                                    visible_when = 'ele13 == "3-Li" and enr13 == "Enriched"'),
                                               Item(name = 'B10_13', label = 'B-10 fraction',
                                                    visible_when = 'ele13 == "5-B" and enr13 == "Enriched"'),
                                               Item(name = 'B11_13', label = 'B-11 fraction',
                                                    visible_when = 'ele13 == "5-B" and enr13 == "Enriched"'),
                                               Item(name = 'U234_13', label = 'U-234 fraction',
                                                    visible_when = 'ele13 == "92-U" and enr13 == "Enriched"'),
                                               Item(name = 'U235_13', label = 'U-235 fraction',
                                                    visible_when = 'ele13 == "92-U" and enr13 == "Enriched"'),
                                               Item(name = 'U238_13', label = 'U-238 fraction',
                                                    visible_when = 'ele13 == "92-U" and enr13 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 12'
                                             ),
                                        Group(
                                               Item(name = 'ele14', label = 'Element', width = -100),
                                               Item(name = 'frac14', label = 'Fraction'),
                                               Item(name = 'enr14', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele14 == "3-Li" or ele14 == "5-B" or ele14 == "92-U"'),
                                               Item(name = 'Li6_14', label = 'Li-6 fraction',
                                                    visible_when = 'ele14 == "3-Li" and enr14 == "Enriched"'),
                                               Item(name = 'Li7_14', label = 'Li-7 fraction',
                                                    visible_when = 'ele14 == "3-Li" and enr14 == "Enriched"'),
                                               Item(name = 'B10_14', label = 'B-10 fraction',
                                                    visible_when = 'ele14 == "5-B" and enr14 == "Enriched"'),
                                               Item(name = 'B11_14', label = 'B-11 fraction',
                                                    visible_when = 'ele14 == "5-B" and enr14 == "Enriched"'),
                                               Item(name = 'U234_14', label = 'U-234 fraction',
                                                    visible_when = 'ele14 == "92-U" and enr14 == "Enriched"'),
                                               Item(name = 'U235_14', label = 'U-235 fraction',
                                                    visible_when = 'ele14 == "92-U" and enr14 == "Enriched"'),
                                               Item(name = 'U238_14', label = 'U-238 fraction',
                                                    visible_when = 'ele14 == "92-U" and enr14 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 13'
                                             ),
                                        Group(
                                               Item(name = 'ele15', label = 'Element', width = -100),
                                               Item(name = 'frac15', label = 'Fraction'),
                                               Item(name = 'enr15', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele15 == "3-Li" or ele15 == "5-B" or ele15 == "92-U"'),
                                               Item(name = 'Li6_15', label = 'Li-6 fraction',
                                                    visible_when = 'ele15 == "3-Li" and enr15 == "Enriched"'),
                                               Item(name = 'Li7_15', label = 'Li-7 fraction',
                                                    visible_when = 'ele15 == "3-Li" and enr15 == "Enriched"'),
                                               Item(name = 'B10_15', label = 'B-10 fraction',
                                                    visible_when = 'ele15 == "5-B" and enr15 == "Enriched"'),
                                               Item(name = 'B11_15', label = 'B-11 fraction',
                                                    visible_when = 'ele15 == "5-B" and enr15 == "Enriched"'),
                                               Item(name = 'U234_15', label = 'U-234 fraction',
                                                    visible_when = 'ele15 == "92-U" and enr15 == "Enriched"'),
                                               Item(name = 'U235_15', label = 'U-235 fraction',
                                                    visible_when = 'ele15 == "92-U" and enr15 == "Enriched"'),
                                               Item(name = 'U238_15', label = 'U-238 fraction',
                                                    visible_when = 'ele15 == "92-U" and enr15 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 14'
                                             ),
                                        Group(
                                               Item(name = 'ele16', label = 'Element', width = -100),
                                               Item(name = 'frac16', label = 'Fraction'),
                                               Item(name = 'enr16', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele16 == "3-Li" or ele16 == "5-B" or ele16 == "92-U"'),
                                               Item(name = 'Li6_16', label = 'Li-6 fraction',
                                                    visible_when = 'ele16 == "3-Li" and enr16 == "Enriched"'),
                                               Item(name = 'Li7_16', label = 'Li-7 fraction',
                                                    visible_when = 'ele16 == "3-Li" and enr16 == "Enriched"'),
                                               Item(name = 'B10_16', label = 'B-10 fraction',
                                                    visible_when = 'ele16 == "5-B" and enr16 == "Enriched"'),
                                               Item(name = 'B11_16', label = 'B-11 fraction',
                                                    visible_when = 'ele16 == "5-B" and enr16 == "Enriched"'),
                                               Item(name = 'U234_16', label = 'U-234 fraction',
                                                    visible_when = 'ele16 == "92-U" and enr16 == "Enriched"'),
                                               Item(name = 'U235_16', label = 'U-235 fraction',
                                                    visible_when = 'ele16 == "92-U" and enr16 == "Enriched"'),
                                               Item(name = 'U238_16', label = 'U-238 fraction',
                                                    visible_when = 'ele16 == "92-U" and enr16 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 15'
                                             ),
                                        Group(
                                               Item(name = 'ele17', label = 'Element', width = -100),
                                               Item(name = 'frac17', label = 'Fraction'),
                                               Item(name = 'enr17', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele17 == "3-Li" or ele17 == "5-B" or ele17 == "92-U"'),
                                               Item(name = 'Li6_17', label = 'Li-6 fraction',
                                                    visible_when = 'ele17 == "3-Li" and enr17 == "Enriched"'),
                                               Item(name = 'Li7_17', label = 'Li-7 fraction',
                                                    visible_when = 'ele17 == "3-Li" and enr17 == "Enriched"'),
                                               Item(name = 'B10_17', label = 'B-10 fraction',
                                                    visible_when = 'ele17 == "5-B" and enr17 == "Enriched"'),
                                               Item(name = 'B11_17', label = 'B-11 fraction',
                                                    visible_when = 'ele17 == "5-B" and enr17 == "Enriched"'),
                                               Item(name = 'U234_17', label = 'U-234 fraction',
                                                    visible_when = 'ele17 == "92-U" and enr17 == "Enriched"'),
                                               Item(name = 'U235_17', label = 'U-235 fraction',
                                                    visible_when = 'ele17 == "92-U" and enr17 == "Enriched"'),
                                               Item(name = 'U238_17', label = 'U-238 fraction',
                                                    visible_when = 'ele17 == "92-U" and enr17 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 16'
                                             ),
                                        Group(
                                               Item(name = 'ele18', label = 'Element', width = -100),
                                               Item(name = 'frac18', label = 'Fraction'),
                                               Item(name = 'enr18', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele18 == "3-Li" or ele18 == "5-B" or ele18 == "92-U"'),
                                               Item(name = 'Li6_18', label = 'Li-6 fraction',
                                                    visible_when = 'ele18 == "3-Li" and enr18 == "Enriched"'),
                                               Item(name = 'Li7_18', label = 'Li-7 fraction',
                                                    visible_when = 'ele18 == "3-Li" and enr18 == "Enriched"'),
                                               Item(name = 'B10_18', label = 'B-10 fraction',
                                                    visible_when = 'ele18 == "5-B" and enr18 == "Enriched"'),
                                               Item(name = 'B11_18', label = 'B-11 fraction',
                                                    visible_when = 'ele18 == "5-B" and enr18 == "Enriched"'),
                                               Item(name = 'U234_18', label = 'U-234 fraction',
                                                    visible_when = 'ele18 == "92-U" and enr18 == "Enriched"'),
                                               Item(name = 'U235_18', label = 'U-235 fraction',
                                                    visible_when = 'ele18 == "92-U" and enr18 == "Enriched"'),
                                               Item(name = 'U238_18', label = 'U-238 fraction',
                                                    visible_when = 'ele18 == "92-U" and enr18 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 17'
                                             ),
                                        Group(
                                               Item(name = 'ele19', label = 'Element', width = -100),
                                               Item(name = 'frac19', label = 'Fraction'),
                                               Item(name = 'enr19', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele19 == "3-Li" or ele19 == "5-B" or ele19 == "92-U"'),
                                               Item(name = 'Li6_19', label = 'Li-6 fraction',
                                                    visible_when = 'ele19 == "3-Li" and enr19 == "Enriched"'),
                                               Item(name = 'Li7_19', label = 'Li-7 fraction',
                                                    visible_when = 'ele19 == "3-Li" and enr19 == "Enriched"'),
                                               Item(name = 'B10_19', label = 'B-10 fraction',
                                                    visible_when = 'ele19 == "5-B" and enr19 == "Enriched"'),
                                               Item(name = 'B11_19', label = 'B-11 fraction',
                                                    visible_when = 'ele19 == "5-B" and enr19 == "Enriched"'),
                                               Item(name = 'U234_19', label = 'U-234 fraction',
                                                    visible_when = 'ele19 == "92-U" and enr19 == "Enriched"'),
                                               Item(name = 'U235_19', label = 'U-235 fraction',
                                                    visible_when = 'ele19 == "92-U" and enr19 == "Enriched"'),
                                               Item(name = 'U238_19', label = 'U-238 fraction',
                                                    visible_when = 'ele19 == "92-U" and enr19 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 18'
                                             ),
                                        Group(
                                               Item(name = 'ele20', label = 'Element', width = -100),
                                               Item(name = 'frac20', label = 'Fraction'),
                                               Item(name = 'enr20', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele20 == "3-Li" or ele20 == "5-B" or ele20 == "92-U"'),
                                               Item(name = 'Li6_20', label = 'Li-6 fraction',
                                                    visible_when = 'ele20 == "3-Li" and enr20 == "Enriched"'),
                                               Item(name = 'Li7_20', label = 'Li-7 fraction',
                                                    visible_when = 'ele20 == "3-Li" and enr20 == "Enriched"'),
                                               Item(name = 'B10_20', label = 'B-10 fraction',
                                                    visible_when = 'ele20 == "5-B" and enr20 == "Enriched"'),
                                               Item(name = 'B11_20', label = 'B-11 fraction',
                                                    visible_when = 'ele20 == "5-B" and enr20 == "Enriched"'),
                                               Item(name = 'U234_20', label = 'U-234 fraction',
                                                    visible_when = 'ele20 == "92-U" and enr20 == "Enriched"'),
                                               Item(name = 'U235_20', label = 'U-235 fraction',
                                                    visible_when = 'ele20 == "92-U" and enr20 == "Enriched"'),
                                               Item(name = 'U238_20', label = 'U-238 fraction',
                                                    visible_when = 'ele20 == "92-U" and enr20 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 19'
                                             ),
                                        Group(
                                               Item(name = 'ele21', label = 'Element', width = -100),
                                               Item(name = 'frac21', label = 'Fraction'),
                                               Item(name = 'enr21', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele21 == "3-Li" or ele21 == "5-B" or ele21 == "92-U"'),
                                               Item(name = 'Li6_21', label = 'Li-6 fraction',
                                                    visible_when = 'ele21 == "3-Li" and enr21 == "Enriched"'),
                                               Item(name = 'Li7_21', label = 'Li-7 fraction',
                                                    visible_when = 'ele21 == "3-Li" and enr21 == "Enriched"'),
                                               Item(name = 'B10_21', label = 'B-10 fraction',
                                                    visible_when = 'ele21 == "5-B" and enr21 == "Enriched"'),
                                               Item(name = 'B11_21', label = 'B-11 fraction',
                                                    visible_when = 'ele21 == "5-B" and enr21 == "Enriched"'),
                                               Item(name = 'U234_21', label = 'U-234 fraction',
                                                    visible_when = 'ele21 == "92-U" and enr21 == "Enriched"'),
                                               Item(name = 'U235_21', label = 'U-235 fraction',
                                                    visible_when = 'ele21 == "92-U" and enr21 == "Enriched"'),
                                               Item(name = 'U238_21', label = 'U-238 fraction',
                                                    visible_when = 'ele21 == "92-U" and enr21 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 20'
                                             ),
                                        Group(
                                               Item(name = 'ele22', label = 'Element', width = -100),
                                               Item(name = 'frac22', label = 'Fraction'),
                                               Item(name = 'enr22', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele22 == "3-Li" or ele22 == "5-B" or ele22 == "92-U"'),
                                               Item(name = 'Li6_22', label = 'Li-6 fraction',
                                                    visible_when = 'ele22 == "3-Li" and enr22 == "Enriched"'),
                                               Item(name = 'Li7_22', label = 'Li-7 fraction',
                                                    visible_when = 'ele22 == "3-Li" and enr22 == "Enriched"'),
                                               Item(name = 'B10_22', label = 'B-10 fraction',
                                                    visible_when = 'ele22 == "5-B" and enr22 == "Enriched"'),
                                               Item(name = 'B11_22', label = 'B-11 fraction',
                                                    visible_when = 'ele22 == "5-B" and enr22 == "Enriched"'),
                                               Item(name = 'U234_22', label = 'U-234 fraction',
                                                    visible_when = 'ele22 == "92-U" and enr22 == "Enriched"'),
                                               Item(name = 'U235_22', label = 'U-235 fraction',
                                                    visible_when = 'ele22 == "92-U" and enr22 == "Enriched"'),
                                               Item(name = 'U238_22', label = 'U-238 fraction',
                                                    visible_when = 'ele22 == "92-U" and enr22 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 21'
                                             ),
                                        Group(
                                               Item(name = 'ele23', label = 'Element', width = -100),
                                               Item(name = 'frac23', label = 'Fraction'),
                                               Item(name = 'enr23', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele23 == "3-Li" or ele23 == "5-B" or ele23 == "92-U"'),
                                               Item(name = 'Li6_23', label = 'Li-6 fraction',
                                                    visible_when = 'ele23 == "3-Li" and enr23 == "Enriched"'),
                                               Item(name = 'Li7_23', label = 'Li-7 fraction',
                                                    visible_when = 'ele23 == "3-Li" and enr23 == "Enriched"'),
                                               Item(name = 'B10_23', label = 'B-10 fraction',
                                                    visible_when = 'ele23 == "5-B" and enr23 == "Enriched"'),
                                               Item(name = 'B11_23', label = 'B-11 fraction',
                                                    visible_when = 'ele23 == "5-B" and enr23 == "Enriched"'),
                                               Item(name = 'U234_23', label = 'U-234 fraction',
                                                    visible_when = 'ele23 == "92-U" and enr23 == "Enriched"'),
                                               Item(name = 'U235_23', label = 'U-235 fraction',
                                                    visible_when = 'ele23 == "92-U" and enr23 == "Enriched"'),
                                               Item(name = 'U238_23', label = 'U-238 fraction',
                                                    visible_when = 'ele23 == "92-U" and enr23 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 22'
                                             ),
                                        Group(
                                               Item(name = 'ele24', label = 'Element', width = -100),
                                               Item(name = 'frac24', label = 'Fraction'),
                                               Item(name = 'enr24', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele24 == "3-Li" or ele24 == "5-B" or ele24 == "92-U"'),
                                               Item(name = 'Li6_24', label = 'Li-6 fraction',
                                                    visible_when = 'ele24 == "3-Li" and enr24 == "Enriched"'),
                                               Item(name = 'Li7_24', label = 'Li-7 fraction',
                                                    visible_when = 'ele24 == "3-Li" and enr24 == "Enriched"'),
                                               Item(name = 'B10_24', label = 'B-10 fraction',
                                                    visible_when = 'ele24 == "5-B" and enr24 == "Enriched"'),
                                               Item(name = 'B11_24', label = 'B-11 fraction',
                                                    visible_when = 'ele24 == "5-B" and enr24 == "Enriched"'),
                                               Item(name = 'U234_24', label = 'U-234 fraction',
                                                    visible_when = 'ele24 == "92-U" and enr24 == "Enriched"'),
                                               Item(name = 'U235_24', label = 'U-235 fraction',
                                                    visible_when = 'ele24 == "92-U" and enr24 == "Enriched"'),
                                               Item(name = 'U238_24', label = 'U-238 fraction',
                                                    visible_when = 'ele24 == "92-U" and enr24 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 23'
                                             ),
                                        Group(
                                               Item(name = 'ele25', label = 'Element', width = -100),
                                               Item(name = 'frac25', label = 'Fraction'),
                                               Item(name = 'enr25', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele25 == "3-Li" or ele25 == "5-B" or ele25 == "92-U"'),
                                               Item(name = 'Li6_25', label = 'Li-6 fraction',
                                                    visible_when = 'ele25 == "3-Li" and enr25 == "Enriched"'),
                                               Item(name = 'Li7_25', label = 'Li-7 fraction',
                                                    visible_when = 'ele25 == "3-Li" and enr25 == "Enriched"'),
                                               Item(name = 'B10_25', label = 'B-10 fraction',
                                                    visible_when = 'ele25 == "5-B" and enr25 == "Enriched"'),
                                               Item(name = 'B11_25', label = 'B-11 fraction',
                                                    visible_when = 'ele25 == "5-B" and enr25 == "Enriched"'),
                                               Item(name = 'U234_25', label = 'U-234 fraction',
                                                    visible_when = 'ele25 == "92-U" and enr25 == "Enriched"'),
                                               Item(name = 'U235_25', label = 'U-235 fraction',
                                                    visible_when = 'ele25 == "92-U" and enr25 == "Enriched"'),
                                               Item(name = 'U238_25', label = 'U-238 fraction',
                                                    visible_when = 'ele25 == "92-U" and enr25 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 24'
                                             ),
                                        Group(
                                               Item(name = 'ele26', label = 'Element', width = -100),
                                               Item(name = 'frac26', label = 'Fraction'),
                                               Item(name = 'enr26', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele26 == "3-Li" or ele26 == "5-B" or ele26 == "92-U"'),
                                               Item(name = 'Li6_26', label = 'Li-6 fraction',
                                                    visible_when = 'ele26 == "3-Li" and enr26 == "Enriched"'),
                                               Item(name = 'Li7_26', label = 'Li-7 fraction',
                                                    visible_when = 'ele26 == "3-Li" and enr26 == "Enriched"'),
                                               Item(name = 'B10_26', label = 'B-10 fraction',
                                                    visible_when = 'ele26 == "5-B" and enr26 == "Enriched"'),
                                               Item(name = 'B11_26', label = 'B-11 fraction',
                                                    visible_when = 'ele26 == "5-B" and enr26 == "Enriched"'),
                                               Item(name = 'U234_26', label = 'U-234 fraction',
                                                    visible_when = 'ele26 == "92-U" and enr26 == "Enriched"'),
                                               Item(name = 'U235_26', label = 'U-235 fraction',
                                                    visible_when = 'ele26 == "92-U" and enr26 == "Enriched"'),
                                               Item(name = 'U238_26', label = 'U-238 fraction',
                                                    visible_when = 'ele26 == "92-U" and enr26 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 25'
                                             ),
                                        Group(
                                               Item(name = 'ele27', label = 'Element', width = -100),
                                               Item(name = 'frac27', label = 'Fraction'),
                                               Item(name = 'enr27', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele27 == "3-Li" or ele27 == "5-B" or ele27 == "92-U"'),
                                               Item(name = 'Li6_27', label = 'Li-6 fraction',
                                                    visible_when = 'ele27 == "3-Li" and enr27 == "Enriched"'),
                                               Item(name = 'Li7_27', label = 'Li-7 fraction',
                                                    visible_when = 'ele27 == "3-Li" and enr27 == "Enriched"'),
                                               Item(name = 'B10_27', label = 'B-10 fraction',
                                                    visible_when = 'ele27 == "5-B" and enr27 == "Enriched"'),
                                               Item(name = 'B11_27', label = 'B-11 fraction',
                                                    visible_when = 'ele27 == "5-B" and enr27 == "Enriched"'),
                                               Item(name = 'U234_27', label = 'U-234 fraction',
                                                    visible_when = 'ele27 == "92-U" and enr27 == "Enriched"'),
                                               Item(name = 'U235_27', label = 'U-235 fraction',
                                                    visible_when = 'ele27 == "92-U" and enr27 == "Enriched"'),
                                               Item(name = 'U238_27', label = 'U-238 fraction',
                                                    visible_when = 'ele27 == "92-U" and enr27 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 26'
                                             ),
                                        Group(
                                               Item(name = 'ele28', label = 'Element', width = -100),
                                               Item(name = 'frac28', label = 'Fraction'),
                                               Item(name = 'enr28', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele28 == "3-Li" or ele28 == "5-B" or ele28 == "92-U"'),
                                               Item(name = 'Li6_28', label = 'Li-6 fraction',
                                                    visible_when = 'ele28 == "3-Li" and enr28 == "Enriched"'),
                                               Item(name = 'Li7_28', label = 'Li-7 fraction',
                                                    visible_when = 'ele28 == "3-Li" and enr28 == "Enriched"'),
                                               Item(name = 'B10_28', label = 'B-10 fraction',
                                                    visible_when = 'ele28 == "5-B" and enr28 == "Enriched"'),
                                               Item(name = 'B11_28', label = 'B-11 fraction',
                                                    visible_when = 'ele28 == "5-B" and enr28 == "Enriched"'),
                                               Item(name = 'U234_28', label = 'U-234 fraction',
                                                    visible_when = 'ele28 == "92-U" and enr28 == "Enriched"'),
                                               Item(name = 'U235_28', label = 'U-235 fraction',
                                                    visible_when = 'ele28 == "92-U" and enr28 == "Enriched"'),
                                               Item(name = 'U238_28', label = 'U-238 fraction',
                                                    visible_when = 'ele28 == "92-U" and enr28 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 27'
                                             ),
                                        Group(
                                               Item(name = 'ele29', label = 'Element', width = -100),
                                               Item(name = 'frac29', label = 'Fraction'),
                                               Item(name = 'enr29', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele29 == "3-Li" or ele29 == "5-B" or ele29 == "92-U"'),
                                               Item(name = 'Li6_29', label = 'Li-6 fraction',
                                                    visible_when = 'ele29 == "3-Li" and enr29 == "Enriched"'),
                                               Item(name = 'Li7_29', label = 'Li-7 fraction',
                                                    visible_when = 'ele29 == "3-Li" and enr29 == "Enriched"'),
                                               Item(name = 'B10_29', label = 'B-10 fraction',
                                                    visible_when = 'ele29 == "5-B" and enr29 == "Enriched"'),
                                               Item(name = 'B11_29', label = 'B-11 fraction',
                                                    visible_when = 'ele29 == "5-B" and enr29 == "Enriched"'),
                                               Item(name = 'U234_29', label = 'U-234 fraction',
                                                    visible_when = 'ele29 == "92-U" and enr29 == "Enriched"'),
                                               Item(name = 'U235_29', label = 'U-235 fraction',
                                                    visible_when = 'ele29 == "92-U" and enr29 == "Enriched"'),
                                               Item(name = 'U238_29', label = 'U-238 fraction',
                                                    visible_when = 'ele29 == "92-U" and enr29 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 28'
                                             ),
                                        Group(
                                               Item(name = 'ele30', label = 'Element', width = -100),
                                               Item(name = 'frac30', label = 'Fraction'),
                                               Item(name = 'enr30', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele30 == "3-Li" or ele30 == "5-B" or ele30 == "92-U"'),
                                               Item(name = 'Li6_30', label = 'Li-6 fraction',
                                                    visible_when = 'ele30 == "3-Li" and enr30 == "Enriched"'),
                                               Item(name = 'Li7_30', label = 'Li-7 fraction',
                                                    visible_when = 'ele30 == "3-Li" and enr30 == "Enriched"'),
                                               Item(name = 'B10_30', label = 'B-10 fraction',
                                                    visible_when = 'ele30 == "5-B" and enr30 == "Enriched"'),
                                               Item(name = 'B11_30', label = 'B-11 fraction',
                                                    visible_when = 'ele30 == "5-B" and enr30 == "Enriched"'),
                                               Item(name = 'U234_30', label = 'U-234 fraction',
                                                    visible_when = 'ele30 == "92-U" and enr30 == "Enriched"'),
                                               Item(name = 'U235_30', label = 'U-235 fraction',
                                                    visible_when = 'ele30 == "92-U" and enr30 == "Enriched"'),
                                               Item(name = 'U238_30', label = 'U-238 fraction',
                                                    visible_when = 'ele30 == "92-U" and enr30 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 29'
                                             ),
                                        Group(
                                               Item(name = 'ele31', label = 'Element', width = -100),
                                               Item(name = 'frac31', label = 'Fraction'),
                                               Item(name = 'enr31', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele31 == "3-Li" or ele31 == "5-B" or ele31 == "92-U"'),
                                               Item(name = 'Li6_31', label = 'Li-6 fraction',
                                                    visible_when = 'ele31 == "3-Li" and enr31 == "Enriched"'),
                                               Item(name = 'Li7_31', label = 'Li-7 fraction',
                                                    visible_when = 'ele31 == "3-Li" and enr31 == "Enriched"'),
                                               Item(name = 'B10_31', label = 'B-10 fraction',
                                                    visible_when = 'ele31 == "5-B" and enr31 == "Enriched"'),
                                               Item(name = 'B11_31', label = 'B-11 fraction',
                                                    visible_when = 'ele31 == "5-B" and enr31 == "Enriched"'),
                                               Item(name = 'U234_31', label = 'U-234 fraction',
                                                    visible_when = 'ele31 == "92-U" and enr31 == "Enriched"'),
                                               Item(name = 'U235_31', label = 'U-235 fraction',
                                                    visible_when = 'ele31 == "92-U" and enr31 == "Enriched"'),
                                               Item(name = 'U238_31', label = 'U-238 fraction',
                                                    visible_when = 'ele31 == "92-U" and enr31 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 30'
                                             ),
                                        Group(
                                               Item(name = 'ele32', label = 'Element', width = -100),
                                               Item(name = 'frac32', label = 'Fraction'),
                                               Item(name = 'enr32', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele32 == "3-Li" or ele32 == "5-B" or ele32 == "92-U"'),
                                               Item(name = 'Li6_32', label = 'Li-6 fraction',
                                                    visible_when = 'ele32 == "3-Li" and enr32 == "Enriched"'),
                                               Item(name = 'Li7_32', label = 'Li-7 fraction',
                                                    visible_when = 'ele32 == "3-Li" and enr32 == "Enriched"'),
                                               Item(name = 'B10_32', label = 'B-10 fraction',
                                                    visible_when = 'ele32 == "5-B" and enr32 == "Enriched"'),
                                               Item(name = 'B11_32', label = 'B-11 fraction',
                                                    visible_when = 'ele32 == "5-B" and enr32 == "Enriched"'),
                                               Item(name = 'U234_32', label = 'U-234 fraction',
                                                    visible_when = 'ele32 == "92-U" and enr32 == "Enriched"'),
                                               Item(name = 'U235_32', label = 'U-235 fraction',
                                                    visible_when = 'ele32 == "92-U" and enr32 == "Enriched"'),
                                               Item(name = 'U238_32', label = 'U-238 fraction',
                                                    visible_when = 'ele32 == "92-U" and enr32 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 31'
                                             ),
                                        Group(
                                               Item(name = 'ele33', label = 'Element', width = -100),
                                               Item(name = 'frac33', label = 'Fraction'),
                                               Item(name = 'enr33', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele33 == "3-Li" or ele33 == "5-B" or ele33 == "92-U"'),
                                               Item(name = 'Li6_33', label = 'Li-6 fraction',
                                                    visible_when = 'ele33 == "3-Li" and enr33 == "Enriched"'),
                                               Item(name = 'Li7_33', label = 'Li-7 fraction',
                                                    visible_when = 'ele33 == "3-Li" and enr33 == "Enriched"'),
                                               Item(name = 'B10_33', label = 'B-10 fraction',
                                                    visible_when = 'ele33 == "5-B" and enr33 == "Enriched"'),
                                               Item(name = 'B11_33', label = 'B-11 fraction',
                                                    visible_when = 'ele33 == "5-B" and enr33 == "Enriched"'),
                                               Item(name = 'U234_33', label = 'U-234 fraction',
                                                    visible_when = 'ele33 == "92-U" and enr33 == "Enriched"'),
                                               Item(name = 'U235_33', label = 'U-235 fraction',
                                                    visible_when = 'ele33 == "92-U" and enr33 == "Enriched"'),
                                               Item(name = 'U238_33', label = 'U-238 fraction',
                                                    visible_when = 'ele33 == "92-U" and enr33 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 32'
                                             ),
                                        Group(
                                               Item(name = 'ele34', label = 'Element', width = -100),
                                               Item(name = 'frac34', label = 'Fraction'),
                                               Item(name = 'enr34', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele34 == "3-Li" or ele34 == "5-B" or ele34 == "92-U"'),
                                               Item(name = 'Li6_34', label = 'Li-6 fraction',
                                                    visible_when = 'ele34 == "3-Li" and enr34 == "Enriched"'),
                                               Item(name = 'Li7_34', label = 'Li-7 fraction',
                                                    visible_when = 'ele34 == "3-Li" and enr34 == "Enriched"'),
                                               Item(name = 'B10_34', label = 'B-10 fraction',
                                                    visible_when = 'ele34 == "5-B" and enr34 == "Enriched"'),
                                               Item(name = 'B11_34', label = 'B-11 fraction',
                                                    visible_when = 'ele34 == "5-B" and enr34 == "Enriched"'),
                                               Item(name = 'U234_34', label = 'U-234 fraction',
                                                    visible_when = 'ele34 == "92-U" and enr34 == "Enriched"'),
                                               Item(name = 'U235_34', label = 'U-235 fraction',
                                                    visible_when = 'ele34 == "92-U" and enr34 == "Enriched"'),
                                               Item(name = 'U238_34', label = 'U-238 fraction',
                                                    visible_when = 'ele34 == "92-U" and enr34 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 33'
                                             ),
                                        Group(
                                               Item(name = 'ele35', label = 'Element', width = -100),
                                               Item(name = 'frac35', label = 'Fraction'),
                                               Item(name = 'enr35', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele35 == "3-Li" or ele35 == "5-B" or ele35 == "92-U"'),
                                               Item(name = 'Li6_35', label = 'Li-6 fraction',
                                                    visible_when = 'ele35 == "3-Li" and enr35 == "Enriched"'),
                                               Item(name = 'Li7_35', label = 'Li-7 fraction',
                                                    visible_when = 'ele35 == "3-Li" and enr35 == "Enriched"'),
                                               Item(name = 'B10_35', label = 'B-10 fraction',
                                                    visible_when = 'ele35 == "5-B" and enr35 == "Enriched"'),
                                               Item(name = 'B11_35', label = 'B-11 fraction',
                                                    visible_when = 'ele35 == "5-B" and enr35 == "Enriched"'),
                                               Item(name = 'U234_35', label = 'U-234 fraction',
                                                    visible_when = 'ele35 == "92-U" and enr35 == "Enriched"'),
                                               Item(name = 'U235_35', label = 'U-235 fraction',
                                                    visible_when = 'ele35 == "92-U" and enr35 == "Enriched"'),
                                               Item(name = 'U238_35', label = 'U-238 fraction',
                                                    visible_when = 'ele35 == "92-U" and enr35 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 34'
                                             ),
                                        Group(
                                               Item(name = 'ele36', label = 'Element', width = -100),
                                               Item(name = 'frac36', label = 'Fraction'),
                                               Item(name = 'enr36', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele36 == "3-Li" or ele36 == "5-B" or ele36 == "92-U"'),
                                               Item(name = 'Li6_36', label = 'Li-6 fraction',
                                                    visible_when = 'ele36 == "3-Li" and enr36 == "Enriched"'),
                                               Item(name = 'Li7_36', label = 'Li-7 fraction',
                                                    visible_when = 'ele36 == "3-Li" and enr36 == "Enriched"'),
                                               Item(name = 'B10_36', label = 'B-10 fraction',
                                                    visible_when = 'ele36 == "5-B" and enr36 == "Enriched"'),
                                               Item(name = 'B11_36', label = 'B-11 fraction',
                                                    visible_when = 'ele36 == "5-B" and enr36 == "Enriched"'),
                                               Item(name = 'U234_36', label = 'U-234 fraction',
                                                    visible_when = 'ele36 == "92-U" and enr36 == "Enriched"'),
                                               Item(name = 'U235_36', label = 'U-235 fraction',
                                                    visible_when = 'ele36 == "92-U" and enr36 == "Enriched"'),
                                               Item(name = 'U238_36', label = 'U-238 fraction',
                                                    visible_when = 'ele36 == "92-U" and enr36 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 35'
                                             ),
                                        Group(
                                               Item(name = 'ele37', label = 'Element', width = -100),
                                               Item(name = 'frac37', label = 'Fraction'),
                                               Item(name = 'enr37', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele37 == "3-Li" or ele37 == "5-B" or ele37 == "92-U"'),
                                               Item(name = 'Li6_37', label = 'Li-6 fraction',
                                                    visible_when = 'ele37 == "3-Li" and enr37 == "Enriched"'),
                                               Item(name = 'Li7_37', label = 'Li-7 fraction',
                                                    visible_when = 'ele37 == "3-Li" and enr37 == "Enriched"'),
                                               Item(name = 'B10_37', label = 'B-10 fraction',
                                                    visible_when = 'ele37 == "5-B" and enr37 == "Enriched"'),
                                               Item(name = 'B11_37', label = 'B-11 fraction',
                                                    visible_when = 'ele37 == "5-B" and enr37 == "Enriched"'),
                                               Item(name = 'U234_37', label = 'U-234 fraction',
                                                    visible_when = 'ele37 == "92-U" and enr37 == "Enriched"'),
                                               Item(name = 'U235_37', label = 'U-235 fraction',
                                                    visible_when = 'ele37 == "92-U" and enr37 == "Enriched"'),
                                               Item(name = 'U238_37', label = 'U-238 fraction',
                                                    visible_when = 'ele37 == "92-U" and enr37 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 36'
                                             ),
                                        Group(
                                               Item(name = 'ele38', label = 'Element', width = -100),
                                               Item(name = 'frac38', label = 'Fraction'),
                                               Item(name = 'enr38', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele38 == "3-Li" or ele38 == "5-B" or ele38 == "92-U"'),
                                               Item(name = 'Li6_38', label = 'Li-6 fraction',
                                                    visible_when = 'ele38 == "3-Li" and enr38 == "Enriched"'),
                                               Item(name = 'Li7_38', label = 'Li-7 fraction',
                                                    visible_when = 'ele38 == "3-Li" and enr38 == "Enriched"'),
                                               Item(name = 'B10_38', label = 'B-10 fraction',
                                                    visible_when = 'ele38 == "5-B" and enr38 == "Enriched"'),
                                               Item(name = 'B11_38', label = 'B-11 fraction',
                                                    visible_when = 'ele38 == "5-B" and enr38 == "Enriched"'),
                                               Item(name = 'U234_38', label = 'U-234 fraction',
                                                    visible_when = 'ele38 == "92-U" and enr38 == "Enriched"'),
                                               Item(name = 'U235_38', label = 'U-235 fraction',
                                                    visible_when = 'ele38 == "92-U" and enr38 == "Enriched"'),
                                               Item(name = 'U238_38', label = 'U-238 fraction',
                                                    visible_when = 'ele38 == "92-U" and enr38 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 37'
                                             ),
                                        Group(
                                               Item(name = 'ele39', label = 'Element', width = -100),
                                               Item(name = 'frac39', label = 'Fraction'),
                                               Item(name = 'enr39', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele39 == "3-Li" or ele39 == "5-B" or ele39 == "92-U"'),
                                               Item(name = 'Li6_39', label = 'Li-6 fraction',
                                                    visible_when = 'ele39 == "3-Li" and enr39 == "Enriched"'),
                                               Item(name = 'Li7_39', label = 'Li-7 fraction',
                                                    visible_when = 'ele39 == "3-Li" and enr39 == "Enriched"'),
                                               Item(name = 'B10_39', label = 'B-10 fraction',
                                                    visible_when = 'ele39 == "5-B" and enr39 == "Enriched"'),
                                               Item(name = 'B11_39', label = 'B-11 fraction',
                                                    visible_when = 'ele39 == "5-B" and enr39 == "Enriched"'),
                                               Item(name = 'U234_39', label = 'U-234 fraction',
                                                    visible_when = 'ele39 == "92-U" and enr39 == "Enriched"'),
                                               Item(name = 'U235_39', label = 'U-235 fraction',
                                                    visible_when = 'ele39 == "92-U" and enr39 == "Enriched"'),
                                               Item(name = 'U238_39', label = 'U-238 fraction',
                                                    visible_when = 'ele39 == "92-U" and enr39 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 38'
                                             ),
                                        Group(
                                               Item(name = 'ele40', label = 'Element', width = -100),
                                               Item(name = 'frac40', label = 'Fraction'),
                                               Item(name = 'enr40', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele40 == "3-Li" or ele40 == "5-B" or ele40 == "92-U"'),
                                               Item(name = 'Li6_40', label = 'Li-6 fraction',
                                                    visible_when = 'ele40 == "3-Li" and enr40 == "Enriched"'),
                                               Item(name = 'Li7_40', label = 'Li-7 fraction',
                                                    visible_when = 'ele40 == "3-Li" and enr40 == "Enriched"'),
                                               Item(name = 'B10_40', label = 'B-10 fraction',
                                                    visible_when = 'ele40 == "5-B" and enr40 == "Enriched"'),
                                               Item(name = 'B11_40', label = 'B-11 fraction',
                                                    visible_when = 'ele40 == "5-B" and enr40 == "Enriched"'),
                                               Item(name = 'U234_40', label = 'U-234 fraction',
                                                    visible_when = 'ele40 == "92-U" and enr40 == "Enriched"'),
                                               Item(name = 'U235_40', label = 'U-235 fraction',
                                                    visible_when = 'ele40 == "92-U" and enr40 == "Enriched"'),
                                               Item(name = 'U238_40', label = 'U-238 fraction',
                                                    visible_when = 'ele40 == "92-U" and enr40 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 39'
                                             ),
                                        Group(
                                               Item(name = 'ele41', label = 'Element', width = -100),
                                               Item(name = 'frac41', label = 'Fraction'),
                                               Item(name = 'enr41', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele41 == "3-Li" or ele41 == "5-B" or ele41 == "92-U"'),
                                               Item(name = 'Li6_41', label = 'Li-6 fraction',
                                                    visible_when = 'ele41 == "3-Li" and enr41 == "Enriched"'),
                                               Item(name = 'Li7_41', label = 'Li-7 fraction',
                                                    visible_when = 'ele41 == "3-Li" and enr41 == "Enriched"'),
                                               Item(name = 'B10_41', label = 'B-10 fraction',
                                                    visible_when = 'ele41 == "5-B" and enr41 == "Enriched"'),
                                               Item(name = 'B11_41', label = 'B-11 fraction',
                                                    visible_when = 'ele41 == "5-B" and enr41 == "Enriched"'),
                                               Item(name = 'U234_41', label = 'U-234 fraction',
                                                    visible_when = 'ele41 == "92-U" and enr41 == "Enriched"'),
                                               Item(name = 'U235_41', label = 'U-235 fraction',
                                                    visible_when = 'ele41 == "92-U" and enr41 == "Enriched"'),
                                               Item(name = 'U238_41', label = 'U-238 fraction',
                                                    visible_when = 'ele41 == "92-U" and enr41 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 40'
                                             ),
                                        Group(
                                               Item(name = 'ele42', label = 'Element', width = -100),
                                               Item(name = 'frac42', label = 'Fraction'),
                                               Item(name = 'enr42', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele42 == "3-Li" or ele42 == "5-B" or ele42 == "92-U"'),
                                               Item(name = 'Li6_42', label = 'Li-6 fraction',
                                                    visible_when = 'ele42 == "3-Li" and enr42 == "Enriched"'),
                                               Item(name = 'Li7_42', label = 'Li-7 fraction',
                                                    visible_when = 'ele42 == "3-Li" and enr42 == "Enriched"'),
                                               Item(name = 'B10_42', label = 'B-10 fraction',
                                                    visible_when = 'ele42 == "5-B" and enr42 == "Enriched"'),
                                               Item(name = 'B11_42', label = 'B-11 fraction',
                                                    visible_when = 'ele42 == "5-B" and enr42 == "Enriched"'),
                                               Item(name = 'U234_42', label = 'U-234 fraction',
                                                    visible_when = 'ele42 == "92-U" and enr42 == "Enriched"'),
                                               Item(name = 'U235_42', label = 'U-235 fraction',
                                                    visible_when = 'ele42 == "92-U" and enr42 == "Enriched"'),
                                               Item(name = 'U238_42', label = 'U-238 fraction',
                                                    visible_when = 'ele42 == "92-U" and enr42 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 41'
                                             ),
                                        Group(
                                               Item(name = 'ele43', label = 'Element', width = -100),
                                               Item(name = 'frac43', label = 'Fraction'),
                                               Item(name = 'enr43', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele43 == "3-Li" or ele43 == "5-B" or ele43 == "92-U"'),
                                               Item(name = 'Li6_43', label = 'Li-6 fraction',
                                                    visible_when = 'ele43 == "3-Li" and enr43 == "Enriched"'),
                                               Item(name = 'Li7_43', label = 'Li-7 fraction',
                                                    visible_when = 'ele43 == "3-Li" and enr43 == "Enriched"'),
                                               Item(name = 'B10_43', label = 'B-10 fraction',
                                                    visible_when = 'ele43 == "5-B" and enr43 == "Enriched"'),
                                               Item(name = 'B11_43', label = 'B-11 fraction',
                                                    visible_when = 'ele43 == "5-B" and enr43 == "Enriched"'),
                                               Item(name = 'U234_43', label = 'U-234 fraction',
                                                    visible_when = 'ele43 == "92-U" and enr43 == "Enriched"'),
                                               Item(name = 'U235_43', label = 'U-235 fraction',
                                                    visible_when = 'ele43 == "92-U" and enr43 == "Enriched"'),
                                               Item(name = 'U238_43', label = 'U-238 fraction',
                                                    visible_when = 'ele43 == "92-U" and enr43 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 42'
                                             ),
                                        Group(
                                               Item(name = 'ele44', label = 'Element', width = -100),
                                               Item(name = 'frac44', label = 'Fraction'),
                                               Item(name = 'enr44', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele44 == "3-Li" or ele44 == "5-B" or ele44 == "92-U"'),
                                               Item(name = 'Li6_44', label = 'Li-6 fraction',
                                                    visible_when = 'ele44 == "3-Li" and enr44 == "Enriched"'),
                                               Item(name = 'Li7_44', label = 'Li-7 fraction',
                                                    visible_when = 'ele44 == "3-Li" and enr44 == "Enriched"'),
                                               Item(name = 'B10_44', label = 'B-10 fraction',
                                                    visible_when = 'ele44 == "5-B" and enr44 == "Enriched"'),
                                               Item(name = 'B11_44', label = 'B-11 fraction',
                                                    visible_when = 'ele44 == "5-B" and enr44 == "Enriched"'),
                                               Item(name = 'U234_44', label = 'U-234 fraction',
                                                    visible_when = 'ele44 == "92-U" and enr44 == "Enriched"'),
                                               Item(name = 'U235_44', label = 'U-235 fraction',
                                                    visible_when = 'ele44 == "92-U" and enr44 == "Enriched"'),
                                               Item(name = 'U238_44', label = 'U-238 fraction',
                                                    visible_when = 'ele44 == "92-U" and enr44 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 43'
                                             ),
                                        Group(
                                               Item(name = 'ele45', label = 'Element', width = -100),
                                               Item(name = 'frac45', label = 'Fraction'),
                                               Item(name = 'enr45', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele45 == "3-Li" or ele45 == "5-B" or ele45 == "92-U"'),
                                               Item(name = 'Li6_45', label = 'Li-6 fraction',
                                                    visible_when = 'ele45 == "3-Li" and enr45 == "Enriched"'),
                                               Item(name = 'Li7_45', label = 'Li-7 fraction',
                                                    visible_when = 'ele45 == "3-Li" and enr45 == "Enriched"'),
                                               Item(name = 'B10_45', label = 'B-10 fraction',
                                                    visible_when = 'ele45 == "5-B" and enr45 == "Enriched"'),
                                               Item(name = 'B11_45', label = 'B-11 fraction',
                                                    visible_when = 'ele45 == "5-B" and enr45 == "Enriched"'),
                                               Item(name = 'U234_45', label = 'U-234 fraction',
                                                    visible_when = 'ele45 == "92-U" and enr45 == "Enriched"'),
                                               Item(name = 'U235_45', label = 'U-235 fraction',
                                                    visible_when = 'ele45 == "92-U" and enr45 == "Enriched"'),
                                               Item(name = 'U238_45', label = 'U-238 fraction',
                                                    visible_when = 'ele45 == "92-U" and enr45 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 44'
                                             ),
                                        Group(
                                               Item(name = 'ele46', label = 'Element', width = -100),
                                               Item(name = 'frac46', label = 'Fraction'),
                                               Item(name = 'enr46', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele46 == "3-Li" or ele46 == "5-B" or ele46 == "92-U"'),
                                               Item(name = 'Li6_46', label = 'Li-6 fraction',
                                                    visible_when = 'ele46 == "3-Li" and enr46 == "Enriched"'),
                                               Item(name = 'Li7_46', label = 'Li-7 fraction',
                                                    visible_when = 'ele46 == "3-Li" and enr46 == "Enriched"'),
                                               Item(name = 'B10_46', label = 'B-10 fraction',
                                                    visible_when = 'ele46 == "5-B" and enr46 == "Enriched"'),
                                               Item(name = 'B11_46', label = 'B-11 fraction',
                                                    visible_when = 'ele46 == "5-B" and enr46 == "Enriched"'),
                                               Item(name = 'U234_46', label = 'U-234 fraction',
                                                    visible_when = 'ele46 == "92-U" and enr46 == "Enriched"'),
                                               Item(name = 'U235_46', label = 'U-235 fraction',
                                                    visible_when = 'ele46 == "92-U" and enr46 == "Enriched"'),
                                               Item(name = 'U238_46', label = 'U-238 fraction',
                                                    visible_when = 'ele46 == "92-U" and enr46 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 45'
                                             ),
                                        Group(
                                               Item(name = 'ele47', label = 'Element', width = -100),
                                               Item(name = 'frac47', label = 'Fraction'),
                                               Item(name = 'enr47', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele47 == "3-Li" or ele47 == "5-B" or ele47 == "92-U"'),
                                               Item(name = 'Li6_47', label = 'Li-6 fraction',
                                                    visible_when = 'ele47 == "3-Li" and enr47 == "Enriched"'),
                                               Item(name = 'Li7_47', label = 'Li-7 fraction',
                                                    visible_when = 'ele47 == "3-Li" and enr47 == "Enriched"'),
                                               Item(name = 'B10_47', label = 'B-10 fraction',
                                                    visible_when = 'ele47 == "5-B" and enr47 == "Enriched"'),
                                               Item(name = 'B11_47', label = 'B-11 fraction',
                                                    visible_when = 'ele47 == "5-B" and enr47 == "Enriched"'),
                                               Item(name = 'U234_47', label = 'U-234 fraction',
                                                    visible_when = 'ele47 == "92-U" and enr47 == "Enriched"'),
                                               Item(name = 'U235_47', label = 'U-235 fraction',
                                                    visible_when = 'ele47 == "92-U" and enr47 == "Enriched"'),
                                               Item(name = 'U238_47', label = 'U-238 fraction',
                                                    visible_when = 'ele47 == "92-U" and enr47 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 46'
                                             ),
                                        Group(
                                               Item(name = 'ele48', label = 'Element', width = -100),
                                               Item(name = 'frac48', label = 'Fraction'),
                                               Item(name = 'enr48', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele48 == "3-Li" or ele48 == "5-B" or ele48 == "92-U"'),
                                               Item(name = 'Li6_48', label = 'Li-6 fraction',
                                                    visible_when = 'ele48 == "3-Li" and enr48 == "Enriched"'),
                                               Item(name = 'Li7_48', label = 'Li-7 fraction',
                                                    visible_when = 'ele48 == "3-Li" and enr48 == "Enriched"'),
                                               Item(name = 'B10_48', label = 'B-10 fraction',
                                                    visible_when = 'ele48 == "5-B" and enr48 == "Enriched"'),
                                               Item(name = 'B11_48', label = 'B-11 fraction',
                                                    visible_when = 'ele48 == "5-B" and enr48 == "Enriched"'),
                                               Item(name = 'U234_48', label = 'U-234 fraction',
                                                    visible_when = 'ele48 == "92-U" and enr48 == "Enriched"'),
                                               Item(name = 'U235_48', label = 'U-235 fraction',
                                                    visible_when = 'ele48 == "92-U" and enr48 == "Enriched"'),
                                               Item(name = 'U238_48', label = 'U-238 fraction',
                                                    visible_when = 'ele48 == "92-U" and enr48 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 47'
                                             ),
                                        Group(
                                               Item(name = 'ele49', label = 'Element', width = -100),
                                               Item(name = 'frac49', label = 'Fraction'),
                                               Item(name = 'enr49', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele49 == "3-Li" or ele49 == "5-B" or ele49 == "92-U"'),
                                               Item(name = 'Li6_49', label = 'Li-6 fraction',
                                                    visible_when = 'ele49 == "3-Li" and enr49 == "Enriched"'),
                                               Item(name = 'Li7_49', label = 'Li-7 fraction',
                                                    visible_when = 'ele49 == "3-Li" and enr49 == "Enriched"'),
                                               Item(name = 'B10_49', label = 'B-10 fraction',
                                                    visible_when = 'ele49 == "5-B" and enr49 == "Enriched"'),
                                               Item(name = 'B11_49', label = 'B-11 fraction',
                                                    visible_when = 'ele49 == "5-B" and enr49 == "Enriched"'),
                                               Item(name = 'U234_49', label = 'U-234 fraction',
                                                    visible_when = 'ele49 == "92-U" and enr49 == "Enriched"'),
                                               Item(name = 'U235_49', label = 'U-235 fraction',
                                                    visible_when = 'ele49 == "92-U" and enr49 == "Enriched"'),
                                               Item(name = 'U238_49', label = 'U-238 fraction',
                                                    visible_when = 'ele49 == "92-U" and enr49 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 48'
                                             ),
                                        Group(
                                               Item(name = 'ele50', label = 'Element', width = -100),
                                               Item(name = 'frac50', label = 'Fraction'),
                                               Item(name = 'enr50', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele50 == "3-Li" or ele50 == "5-B" or ele50 == "92-U"'),
                                               Item(name = 'Li6_50', label = 'Li-6 fraction',
                                                    visible_when = 'ele50 == "3-Li" and enr50 == "Enriched"'),
                                               Item(name = 'Li7_50', label = 'Li-7 fraction',
                                                    visible_when = 'ele50 == "3-Li" and enr50 == "Enriched"'),
                                               Item(name = 'B10_50', label = 'B-10 fraction',
                                                    visible_when = 'ele50 == "5-B" and enr50 == "Enriched"'),
                                               Item(name = 'B11_50', label = 'B-11 fraction',
                                                    visible_when = 'ele50 == "5-B" and enr50 == "Enriched"'),
                                               Item(name = 'U234_50', label = 'U-234 fraction',
                                                    visible_when = 'ele50 == "92-U" and enr50 == "Enriched"'),
                                               Item(name = 'U235_50', label = 'U-235 fraction',
                                                    visible_when = 'ele50 == "92-U" and enr50 == "Enriched"'),
                                               Item(name = 'U238_50', label = 'U-238 fraction',
                                                    visible_when = 'ele50 == "92-U" and enr50 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 49'
                                             ),
                                        Group(
                                               Item(name = 'ele51', label = 'Element', width = -100),
                                               Item(name = 'frac51', label = 'Fraction'),
                                               Item(name = 'enr51', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele51 == "3-Li" or ele51 == "5-B" or ele51 == "92-U"'),
                                               Item(name = 'Li6_51', label = 'Li-6 fraction',
                                                    visible_when = 'ele51 == "3-Li" and enr51 == "Enriched"'),
                                               Item(name = 'Li7_51', label = 'Li-7 fraction',
                                                    visible_when = 'ele51 == "3-Li" and enr51 == "Enriched"'),
                                               Item(name = 'B10_51', label = 'B-10 fraction',
                                                    visible_when = 'ele51 == "5-B" and enr51 == "Enriched"'),
                                               Item(name = 'B11_51', label = 'B-11 fraction',
                                                    visible_when = 'ele51 == "5-B" and enr51 == "Enriched"'),
                                               Item(name = 'U234_51', label = 'U-234 fraction',
                                                    visible_when = 'ele51 == "92-U" and enr51 == "Enriched"'),
                                               Item(name = 'U235_51', label = 'U-235 fraction',
                                                    visible_when = 'ele51 == "92-U" and enr51 == "Enriched"'),
                                               Item(name = 'U238_51', label = 'U-238 fraction',
                                                    visible_when = 'ele51 == "92-U" and enr51 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 50'
                                             ),
                                        Group(
                                               Item(name = 'ele52', label = 'Element', width = -100),
                                               Item(name = 'frac52', label = 'Fraction'),
                                               Item(name = 'enr52', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele52 == "3-Li" or ele52 == "5-B" or ele52 == "92-U"'),
                                               Item(name = 'Li6_52', label = 'Li-6 fraction',
                                                    visible_when = 'ele52 == "3-Li" and enr52 == "Enriched"'),
                                               Item(name = 'Li7_52', label = 'Li-7 fraction',
                                                    visible_when = 'ele52 == "3-Li" and enr52 == "Enriched"'),
                                               Item(name = 'B10_52', label = 'B-10 fraction',
                                                    visible_when = 'ele52 == "5-B" and enr52 == "Enriched"'),
                                               Item(name = 'B11_52', label = 'B-11 fraction',
                                                    visible_when = 'ele52 == "5-B" and enr52 == "Enriched"'),
                                               Item(name = 'U234_52', label = 'U-234 fraction',
                                                    visible_when = 'ele52 == "92-U" and enr52 == "Enriched"'),
                                               Item(name = 'U235_52', label = 'U-235 fraction',
                                                    visible_when = 'ele52 == "92-U" and enr52 == "Enriched"'),
                                               Item(name = 'U238_52', label = 'U-238 fraction',
                                                    visible_when = 'ele52 == "92-U" and enr52 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 51'
                                             ),
                                        Group(
                                               Item(name = 'ele53', label = 'Element', width = -100),
                                               Item(name = 'frac53', label = 'Fraction'),
                                               Item(name = 'enr53', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele53 == "3-Li" or ele53 == "5-B" or ele53 == "92-U"'),
                                               Item(name = 'Li6_53', label = 'Li-6 fraction',
                                                    visible_when = 'ele53 == "3-Li" and enr53 == "Enriched"'),
                                               Item(name = 'Li7_53', label = 'Li-7 fraction',
                                                    visible_when = 'ele53 == "3-Li" and enr53 == "Enriched"'),
                                               Item(name = 'B10_53', label = 'B-10 fraction',
                                                    visible_when = 'ele53 == "5-B" and enr53 == "Enriched"'),
                                               Item(name = 'B11_53', label = 'B-11 fraction',
                                                    visible_when = 'ele53 == "5-B" and enr53 == "Enriched"'),
                                               Item(name = 'U234_53', label = 'U-234 fraction',
                                                    visible_when = 'ele53 == "92-U" and enr53 == "Enriched"'),
                                               Item(name = 'U235_53', label = 'U-235 fraction',
                                                    visible_when = 'ele53 == "92-U" and enr53 == "Enriched"'),
                                               Item(name = 'U238_53', label = 'U-238 fraction',
                                                    visible_when = 'ele53 == "92-U" and enr53 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 52'
                                             ),
                                        Group(
                                               Item(name = 'ele54', label = 'Element', width = -100),
                                               Item(name = 'frac54', label = 'Fraction'),
                                               Item(name = 'enr54', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele54 == "3-Li" or ele54 == "5-B" or ele54 == "92-U"'),
                                               Item(name = 'Li6_54', label = 'Li-6 fraction',
                                                    visible_when = 'ele54 == "3-Li" and enr54 == "Enriched"'),
                                               Item(name = 'Li7_54', label = 'Li-7 fraction',
                                                    visible_when = 'ele54 == "3-Li" and enr54 == "Enriched"'),
                                               Item(name = 'B10_54', label = 'B-10 fraction',
                                                    visible_when = 'ele54 == "5-B" and enr54 == "Enriched"'),
                                               Item(name = 'B11_54', label = 'B-11 fraction',
                                                    visible_when = 'ele54 == "5-B" and enr54 == "Enriched"'),
                                               Item(name = 'U234_54', label = 'U-234 fraction',
                                                    visible_when = 'ele54 == "92-U" and enr54 == "Enriched"'),
                                               Item(name = 'U235_54', label = 'U-235 fraction',
                                                    visible_when = 'ele54 == "92-U" and enr54 == "Enriched"'),
                                               Item(name = 'U238_54', label = 'U-238 fraction',
                                                    visible_when = 'ele54 == "92-U" and enr54 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 53'
                                             ),
                                        Group(
                                               Item(name = 'ele55', label = 'Element', width = -100),
                                               Item(name = 'frac55', label = 'Fraction'),
                                               Item(name = 'enr55', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele55 == "3-Li" or ele55 == "5-B" or ele55 == "92-U"'),
                                               Item(name = 'Li6_55', label = 'Li-6 fraction',
                                                    visible_when = 'ele55 == "3-Li" and enr55 == "Enriched"'),
                                               Item(name = 'Li7_55', label = 'Li-7 fraction',
                                                    visible_when = 'ele55 == "3-Li" and enr55 == "Enriched"'),
                                               Item(name = 'B10_55', label = 'B-10 fraction',
                                                    visible_when = 'ele55 == "5-B" and enr55 == "Enriched"'),
                                               Item(name = 'B11_55', label = 'B-11 fraction',
                                                    visible_when = 'ele55 == "5-B" and enr55 == "Enriched"'),
                                               Item(name = 'U234_55', label = 'U-234 fraction',
                                                    visible_when = 'ele55 == "92-U" and enr55 == "Enriched"'),
                                               Item(name = 'U235_55', label = 'U-235 fraction',
                                                    visible_when = 'ele55 == "92-U" and enr55 == "Enriched"'),
                                               Item(name = 'U238_55', label = 'U-238 fraction',
                                                    visible_when = 'ele55 == "92-U" and enr55 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 54'
                                             ),
                                        Group(
                                               Item(name = 'ele56', label = 'Element', width = -100),
                                               Item(name = 'frac56', label = 'Fraction'),
                                               Item(name = 'enr56', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele56 == "3-Li" or ele56 == "5-B" or ele56 == "92-U"'),
                                               Item(name = 'Li6_56', label = 'Li-6 fraction',
                                                    visible_when = 'ele56 == "3-Li" and enr56 == "Enriched"'),
                                               Item(name = 'Li7_56', label = 'Li-7 fraction',
                                                    visible_when = 'ele56 == "3-Li" and enr56 == "Enriched"'),
                                               Item(name = 'B10_56', label = 'B-10 fraction',
                                                    visible_when = 'ele56 == "5-B" and enr56 == "Enriched"'),
                                               Item(name = 'B11_56', label = 'B-11 fraction',
                                                    visible_when = 'ele56 == "5-B" and enr56 == "Enriched"'),
                                               Item(name = 'U234_56', label = 'U-234 fraction',
                                                    visible_when = 'ele56 == "92-U" and enr56 == "Enriched"'),
                                               Item(name = 'U235_56', label = 'U-235 fraction',
                                                    visible_when = 'ele56 == "92-U" and enr56 == "Enriched"'),
                                               Item(name = 'U238_56', label = 'U-238 fraction',
                                                    visible_when = 'ele56 == "92-U" and enr56 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 55'
                                             ),
                                        Group(
                                               Item(name = 'ele57', label = 'Element', width = -100),
                                               Item(name = 'frac57', label = 'Fraction'),
                                               Item(name = 'enr57', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele57 == "3-Li" or ele57 == "5-B" or ele57 == "92-U"'),
                                               Item(name = 'Li6_57', label = 'Li-6 fraction',
                                                    visible_when = 'ele57 == "3-Li" and enr57 == "Enriched"'),
                                               Item(name = 'Li7_57', label = 'Li-7 fraction',
                                                    visible_when = 'ele57 == "3-Li" and enr57 == "Enriched"'),
                                               Item(name = 'B10_57', label = 'B-10 fraction',
                                                    visible_when = 'ele57 == "5-B" and enr57 == "Enriched"'),
                                               Item(name = 'B11_57', label = 'B-11 fraction',
                                                    visible_when = 'ele57 == "5-B" and enr57 == "Enriched"'),
                                               Item(name = 'U234_57', label = 'U-234 fraction',
                                                    visible_when = 'ele57 == "92-U" and enr57 == "Enriched"'),
                                               Item(name = 'U235_57', label = 'U-235 fraction',
                                                    visible_when = 'ele57 == "92-U" and enr57 == "Enriched"'),
                                               Item(name = 'U238_57', label = 'U-238 fraction',
                                                    visible_when = 'ele57 == "92-U" and enr57 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 56'
                                             ),
                                        Group(
                                               Item(name = 'ele58', label = 'Element', width = -100),
                                               Item(name = 'frac58', label = 'Fraction'),
                                               Item(name = 'enr58', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele58 == "3-Li" or ele58 == "5-B" or ele58 == "92-U"'),
                                               Item(name = 'Li6_58', label = 'Li-6 fraction',
                                                    visible_when = 'ele58 == "3-Li" and enr58 == "Enriched"'),
                                               Item(name = 'Li7_58', label = 'Li-7 fraction',
                                                    visible_when = 'ele58 == "3-Li" and enr58 == "Enriched"'),
                                               Item(name = 'B10_58', label = 'B-10 fraction',
                                                    visible_when = 'ele58 == "5-B" and enr58 == "Enriched"'),
                                               Item(name = 'B11_58', label = 'B-11 fraction',
                                                    visible_when = 'ele58 == "5-B" and enr58 == "Enriched"'),
                                               Item(name = 'U234_58', label = 'U-234 fraction',
                                                    visible_when = 'ele58 == "92-U" and enr58 == "Enriched"'),
                                               Item(name = 'U235_58', label = 'U-235 fraction',
                                                    visible_when = 'ele58 == "92-U" and enr58 == "Enriched"'),
                                               Item(name = 'U238_58', label = 'U-238 fraction',
                                                    visible_when = 'ele58 == "92-U" and enr58 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 57'
                                             ),
                                        Group(
                                               Item(name = 'ele59', label = 'Element', width = -100),
                                               Item(name = 'frac59', label = 'Fraction'),
                                               Item(name = 'enr59', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele59 == "3-Li" or ele59 == "5-B" or ele59 == "92-U"'),
                                               Item(name = 'Li6_59', label = 'Li-6 fraction',
                                                    visible_when = 'ele59 == "3-Li" and enr59 == "Enriched"'),
                                               Item(name = 'Li7_59', label = 'Li-7 fraction',
                                                    visible_when = 'ele59 == "3-Li" and enr59 == "Enriched"'),
                                               Item(name = 'B10_59', label = 'B-10 fraction',
                                                    visible_when = 'ele59 == "5-B" and enr59 == "Enriched"'),
                                               Item(name = 'B11_59', label = 'B-11 fraction',
                                                    visible_when = 'ele59 == "5-B" and enr59 == "Enriched"'),
                                               Item(name = 'U234_59', label = 'U-234 fraction',
                                                    visible_when = 'ele59 == "92-U" and enr59 == "Enriched"'),
                                               Item(name = 'U235_59', label = 'U-235 fraction',
                                                    visible_when = 'ele59 == "92-U" and enr59 == "Enriched"'),
                                               Item(name = 'U238_59', label = 'U-238 fraction',
                                                    visible_when = 'ele59 == "92-U" and enr59 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 58'
                                             ),
                                        Group(
                                               Item(name = 'ele60', label = 'Element', width = -100),
                                               Item(name = 'frac60', label = 'Fraction'),
                                               Item(name = 'enr60', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele60 == "3-Li" or ele60 == "5-B" or ele60 == "92-U"'),
                                               Item(name = 'Li6_60', label = 'Li-6 fraction',
                                                    visible_when = 'ele60 == "3-Li" and enr60 == "Enriched"'),
                                               Item(name = 'Li7_60', label = 'Li-7 fraction',
                                                    visible_when = 'ele60 == "3-Li" and enr60 == "Enriched"'),
                                               Item(name = 'B10_60', label = 'B-10 fraction',
                                                    visible_when = 'ele60 == "5-B" and enr60 == "Enriched"'),
                                               Item(name = 'B11_60', label = 'B-11 fraction',
                                                    visible_when = 'ele60 == "5-B" and enr60 == "Enriched"'),
                                               Item(name = 'U234_60', label = 'U-234 fraction',
                                                    visible_when = 'ele60 == "92-U" and enr60 == "Enriched"'),
                                               Item(name = 'U235_60', label = 'U-235 fraction',
                                                    visible_when = 'ele60 == "92-U" and enr60 == "Enriched"'),
                                               Item(name = 'U238_60', label = 'U-238 fraction',
                                                    visible_when = 'ele60 == "92-U" and enr60 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 59'
                                             ),
                                        Group(
                                               Item(name = 'ele61', label = 'Element', width = -100),
                                               Item(name = 'frac61', label = 'Fraction'),
                                               Item(name = 'enr61', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele61 == "3-Li" or ele61 == "5-B" or ele61 == "92-U"'),
                                               Item(name = 'Li6_61', label = 'Li-6 fraction',
                                                    visible_when = 'ele61 == "3-Li" and enr61 == "Enriched"'),
                                               Item(name = 'Li7_61', label = 'Li-7 fraction',
                                                    visible_when = 'ele61 == "3-Li" and enr61 == "Enriched"'),
                                               Item(name = 'B10_61', label = 'B-10 fraction',
                                                    visible_when = 'ele61 == "5-B" and enr61 == "Enriched"'),
                                               Item(name = 'B11_61', label = 'B-11 fraction',
                                                    visible_when = 'ele61 == "5-B" and enr61 == "Enriched"'),
                                               Item(name = 'U234_61', label = 'U-234 fraction',
                                                    visible_when = 'ele61 == "92-U" and enr61 == "Enriched"'),
                                               Item(name = 'U235_61', label = 'U-235 fraction',
                                                    visible_when = 'ele61 == "92-U" and enr61 == "Enriched"'),
                                               Item(name = 'U238_61', label = 'U-238 fraction',
                                                    visible_when = 'ele61 == "92-U" and enr61 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 60'
                                             ),
                                        Group(
                                               Item(name = 'ele62', label = 'Element', width = -100),
                                               Item(name = 'frac62', label = 'Fraction'),
                                               Item(name = 'enr62', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele62 == "3-Li" or ele62 == "5-B" or ele62 == "92-U"'),
                                               Item(name = 'Li6_62', label = 'Li-6 fraction',
                                                    visible_when = 'ele62 == "3-Li" and enr62 == "Enriched"'),
                                               Item(name = 'Li7_62', label = 'Li-7 fraction',
                                                    visible_when = 'ele62 == "3-Li" and enr62 == "Enriched"'),
                                               Item(name = 'B10_62', label = 'B-10 fraction',
                                                    visible_when = 'ele62 == "5-B" and enr62 == "Enriched"'),
                                               Item(name = 'B11_62', label = 'B-11 fraction',
                                                    visible_when = 'ele62 == "5-B" and enr62 == "Enriched"'),
                                               Item(name = 'U234_62', label = 'U-234 fraction',
                                                    visible_when = 'ele62 == "92-U" and enr62 == "Enriched"'),
                                               Item(name = 'U235_62', label = 'U-235 fraction',
                                                    visible_when = 'ele62 == "92-U" and enr62 == "Enriched"'),
                                               Item(name = 'U238_62', label = 'U-238 fraction',
                                                    visible_when = 'ele62 == "92-U" and enr62 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 61'
                                             ),
                                        Group(
                                               Item(name = 'ele63', label = 'Element', width = -100),
                                               Item(name = 'frac63', label = 'Fraction'),
                                               Item(name = 'enr63', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele63 == "3-Li" or ele63 == "5-B" or ele63 == "92-U"'),
                                               Item(name = 'Li6_63', label = 'Li-6 fraction',
                                                    visible_when = 'ele63 == "3-Li" and enr63 == "Enriched"'),
                                               Item(name = 'Li7_63', label = 'Li-7 fraction',
                                                    visible_when = 'ele63 == "3-Li" and enr63 == "Enriched"'),
                                               Item(name = 'B10_63', label = 'B-10 fraction',
                                                    visible_when = 'ele63 == "5-B" and enr63 == "Enriched"'),
                                               Item(name = 'B11_63', label = 'B-11 fraction',
                                                    visible_when = 'ele63 == "5-B" and enr63 == "Enriched"'),
                                               Item(name = 'U234_63', label = 'U-234 fraction',
                                                    visible_when = 'ele63 == "92-U" and enr63 == "Enriched"'),
                                               Item(name = 'U235_63', label = 'U-235 fraction',
                                                    visible_when = 'ele63 == "92-U" and enr63 == "Enriched"'),
                                               Item(name = 'U238_63', label = 'U-238 fraction',
                                                    visible_when = 'ele63 == "92-U" and enr63 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 62'
                                             ),
                                        Group(
                                               Item(name = 'ele64', label = 'Element', width = -100),
                                               Item(name = 'frac64', label = 'Fraction'),
                                               Item(name = 'enr64', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele64 == "3-Li" or ele64 == "5-B" or ele64 == "92-U"'),
                                               Item(name = 'Li6_64', label = 'Li-6 fraction',
                                                    visible_when = 'ele64 == "3-Li" and enr64 == "Enriched"'),
                                               Item(name = 'Li7_64', label = 'Li-7 fraction',
                                                    visible_when = 'ele64 == "3-Li" and enr64 == "Enriched"'),
                                               Item(name = 'B10_64', label = 'B-10 fraction',
                                                    visible_when = 'ele64 == "5-B" and enr64 == "Enriched"'),
                                               Item(name = 'B11_64', label = 'B-11 fraction',
                                                    visible_when = 'ele64 == "5-B" and enr64 == "Enriched"'),
                                               Item(name = 'U234_64', label = 'U-234 fraction',
                                                    visible_when = 'ele64 == "92-U" and enr64 == "Enriched"'),
                                               Item(name = 'U235_64', label = 'U-235 fraction',
                                                    visible_when = 'ele64 == "92-U" and enr64 == "Enriched"'),
                                               Item(name = 'U238_64', label = 'U-238 fraction',
                                                    visible_when = 'ele64 == "92-U" and enr64 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 63'
                                             ),
                                        Group(
                                               Item(name = 'ele65', label = 'Element', width = -100),
                                               Item(name = 'frac65', label = 'Fraction'),
                                               Item(name = 'enr65', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele65 == "3-Li" or ele65 == "5-B" or ele65 == "92-U"'),
                                               Item(name = 'Li6_65', label = 'Li-6 fraction',
                                                    visible_when = 'ele65 == "3-Li" and enr65 == "Enriched"'),
                                               Item(name = 'Li7_65', label = 'Li-7 fraction',
                                                    visible_when = 'ele65 == "3-Li" and enr65 == "Enriched"'),
                                               Item(name = 'B10_65', label = 'B-10 fraction',
                                                    visible_when = 'ele65 == "5-B" and enr65 == "Enriched"'),
                                               Item(name = 'B11_65', label = 'B-11 fraction',
                                                    visible_when = 'ele65 == "5-B" and enr65 == "Enriched"'),
                                               Item(name = 'U234_65', label = 'U-234 fraction',
                                                    visible_when = 'ele65 == "92-U" and enr65 == "Enriched"'),
                                               Item(name = 'U235_65', label = 'U-235 fraction',
                                                    visible_when = 'ele65 == "92-U" and enr65 == "Enriched"'),
                                               Item(name = 'U238_65', label = 'U-238 fraction',
                                                    visible_when = 'ele65 == "92-U" and enr65 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 64'
                                             ),
                                        Group(
                                               Item(name = 'ele66', label = 'Element', width = -100),
                                               Item(name = 'frac66', label = 'Fraction'),
                                               Item(name = 'enr66', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele66 == "3-Li" or ele66 == "5-B" or ele66 == "92-U"'),
                                               Item(name = 'Li6_66', label = 'Li-6 fraction',
                                                    visible_when = 'ele66 == "3-Li" and enr66 == "Enriched"'),
                                               Item(name = 'Li7_66', label = 'Li-7 fraction',
                                                    visible_when = 'ele66 == "3-Li" and enr66 == "Enriched"'),
                                               Item(name = 'B10_66', label = 'B-10 fraction',
                                                    visible_when = 'ele66 == "5-B" and enr66 == "Enriched"'),
                                               Item(name = 'B11_66', label = 'B-11 fraction',
                                                    visible_when = 'ele66 == "5-B" and enr66 == "Enriched"'),
                                               Item(name = 'U234_66', label = 'U-234 fraction',
                                                    visible_when = 'ele66 == "92-U" and enr66 == "Enriched"'),
                                               Item(name = 'U235_66', label = 'U-235 fraction',
                                                    visible_when = 'ele66 == "92-U" and enr66 == "Enriched"'),
                                               Item(name = 'U238_66', label = 'U-238 fraction',
                                                    visible_when = 'ele66 == "92-U" and enr66 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 65'
                                             ),
                                        Group(
                                               Item(name = 'ele67', label = 'Element', width = -100),
                                               Item(name = 'frac67', label = 'Fraction'),
                                               Item(name = 'enr67', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele67 == "3-Li" or ele67 == "5-B" or ele67 == "92-U"'),
                                               Item(name = 'Li6_67', label = 'Li-6 fraction',
                                                    visible_when = 'ele67 == "3-Li" and enr67 == "Enriched"'),
                                               Item(name = 'Li7_67', label = 'Li-7 fraction',
                                                    visible_when = 'ele67 == "3-Li" and enr67 == "Enriched"'),
                                               Item(name = 'B10_67', label = 'B-10 fraction',
                                                    visible_when = 'ele67 == "5-B" and enr67 == "Enriched"'),
                                               Item(name = 'B11_67', label = 'B-11 fraction',
                                                    visible_when = 'ele67 == "5-B" and enr67 == "Enriched"'),
                                               Item(name = 'U234_67', label = 'U-234 fraction',
                                                    visible_when = 'ele67 == "92-U" and enr67 == "Enriched"'),
                                               Item(name = 'U235_67', label = 'U-235 fraction',
                                                    visible_when = 'ele67 == "92-U" and enr67 == "Enriched"'),
                                               Item(name = 'U238_67', label = 'U-238 fraction',
                                                    visible_when = 'ele67 == "92-U" and enr67 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 66'
                                             ),
                                        Group(
                                               Item(name = 'ele68', label = 'Element', width = -100),
                                               Item(name = 'frac68', label = 'Fraction'),
                                               Item(name = 'enr68', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele68 == "3-Li" or ele68 == "5-B" or ele68 == "92-U"'),
                                               Item(name = 'Li6_68', label = 'Li-6 fraction',
                                                    visible_when = 'ele68 == "3-Li" and enr68 == "Enriched"'),
                                               Item(name = 'Li7_68', label = 'Li-7 fraction',
                                                    visible_when = 'ele68 == "3-Li" and enr68 == "Enriched"'),
                                               Item(name = 'B10_68', label = 'B-10 fraction',
                                                    visible_when = 'ele68 == "5-B" and enr68 == "Enriched"'),
                                               Item(name = 'B11_68', label = 'B-11 fraction',
                                                    visible_when = 'ele68 == "5-B" and enr68 == "Enriched"'),
                                               Item(name = 'U234_68', label = 'U-234 fraction',
                                                    visible_when = 'ele68 == "92-U" and enr68 == "Enriched"'),
                                               Item(name = 'U235_68', label = 'U-235 fraction',
                                                    visible_when = 'ele68 == "92-U" and enr68 == "Enriched"'),
                                               Item(name = 'U238_68', label = 'U-238 fraction',
                                                    visible_when = 'ele68 == "92-U" and enr68 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 67'
                                             ),
                                        Group(
                                               Item(name = 'ele69', label = 'Element', width = -100),
                                               Item(name = 'frac69', label = 'Fraction'),
                                               Item(name = 'enr69', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele69 == "3-Li" or ele69 == "5-B" or ele69 == "92-U"'),
                                               Item(name = 'Li6_69', label = 'Li-6 fraction',
                                                    visible_when = 'ele69 == "3-Li" and enr69 == "Enriched"'),
                                               Item(name = 'Li7_69', label = 'Li-7 fraction',
                                                    visible_when = 'ele69 == "3-Li" and enr69 == "Enriched"'),
                                               Item(name = 'B10_69', label = 'B-10 fraction',
                                                    visible_when = 'ele69 == "5-B" and enr69 == "Enriched"'),
                                               Item(name = 'B11_69', label = 'B-11 fraction',
                                                    visible_when = 'ele69 == "5-B" and enr69 == "Enriched"'),
                                               Item(name = 'U234_69', label = 'U-234 fraction',
                                                    visible_when = 'ele69 == "92-U" and enr69 == "Enriched"'),
                                               Item(name = 'U235_69', label = 'U-235 fraction',
                                                    visible_when = 'ele69 == "92-U" and enr69 == "Enriched"'),
                                               Item(name = 'U238_69', label = 'U-238 fraction',
                                                    visible_when = 'ele69 == "92-U" and enr69 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 68'
                                             ),
                                        Group(
                                               Item(name = 'ele70', label = 'Element', width = -100),
                                               Item(name = 'frac70', label = 'Fraction'),
                                               Item(name = 'enr70', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele70 == "3-Li" or ele70 == "5-B" or ele70 == "92-U"'),
                                               Item(name = 'Li6_70', label = 'Li-6 fraction',
                                                    visible_when = 'ele70 == "3-Li" and enr70 == "Enriched"'),
                                               Item(name = 'Li7_70', label = 'Li-7 fraction',
                                                    visible_when = 'ele70 == "3-Li" and enr70 == "Enriched"'),
                                               Item(name = 'B10_70', label = 'B-10 fraction',
                                                    visible_when = 'ele70 == "5-B" and enr70 == "Enriched"'),
                                               Item(name = 'B11_70', label = 'B-11 fraction',
                                                    visible_when = 'ele70 == "5-B" and enr70 == "Enriched"'),
                                               Item(name = 'U234_70', label = 'U-234 fraction',
                                                    visible_when = 'ele70 == "92-U" and enr70 == "Enriched"'),
                                               Item(name = 'U235_70', label = 'U-235 fraction',
                                                    visible_when = 'ele70 == "92-U" and enr70 == "Enriched"'),
                                               Item(name = 'U238_70', label = 'U-238 fraction',
                                                    visible_when = 'ele70 == "92-U" and enr70 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 69'
                                             ),
                                        Group(
                                               Item(name = 'ele71', label = 'Element', width = -100),
                                               Item(name = 'frac71', label = 'Fraction'),
                                               Item(name = 'enr71', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele71 == "3-Li" or ele71 == "5-B" or ele71 == "92-U"'),
                                               Item(name = 'Li6_71', label = 'Li-6 fraction',
                                                    visible_when = 'ele71 == "3-Li" and enr71 == "Enriched"'),
                                               Item(name = 'Li7_71', label = 'Li-7 fraction',
                                                    visible_when = 'ele71 == "3-Li" and enr71 == "Enriched"'),
                                               Item(name = 'B10_71', label = 'B-10 fraction',
                                                    visible_when = 'ele71 == "5-B" and enr71 == "Enriched"'),
                                               Item(name = 'B11_71', label = 'B-11 fraction',
                                                    visible_when = 'ele71 == "5-B" and enr71 == "Enriched"'),
                                               Item(name = 'U234_71', label = 'U-234 fraction',
                                                    visible_when = 'ele71 == "92-U" and enr71 == "Enriched"'),
                                               Item(name = 'U235_71', label = 'U-235 fraction',
                                                    visible_when = 'ele71 == "92-U" and enr71 == "Enriched"'),
                                               Item(name = 'U238_71', label = 'U-238 fraction',
                                                    visible_when = 'ele71 == "92-U" and enr71 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 70'
                                             ),
                                        Group(
                                               Item(name = 'ele72', label = 'Element', width = -100),
                                               Item(name = 'frac72', label = 'Fraction'),
                                               Item(name = 'enr72', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele72 == "3-Li" or ele72 == "5-B" or ele72 == "92-U"'),
                                               Item(name = 'Li6_72', label = 'Li-6 fraction',
                                                    visible_when = 'ele72 == "3-Li" and enr72 == "Enriched"'),
                                               Item(name = 'Li7_72', label = 'Li-7 fraction',
                                                    visible_when = 'ele72 == "3-Li" and enr72 == "Enriched"'),
                                               Item(name = 'B10_72', label = 'B-10 fraction',
                                                    visible_when = 'ele72 == "5-B" and enr72 == "Enriched"'),
                                               Item(name = 'B11_72', label = 'B-11 fraction',
                                                    visible_when = 'ele72 == "5-B" and enr72 == "Enriched"'),
                                               Item(name = 'U234_72', label = 'U-234 fraction',
                                                    visible_when = 'ele72 == "92-U" and enr72 == "Enriched"'),
                                               Item(name = 'U235_72', label = 'U-235 fraction',
                                                    visible_when = 'ele72 == "92-U" and enr72 == "Enriched"'),
                                               Item(name = 'U238_72', label = 'U-238 fraction',
                                                    visible_when = 'ele72 == "92-U" and enr72 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 71'
                                             ),
                                        Group(
                                               Item(name = 'ele73', label = 'Element', width = -100),
                                               Item(name = 'frac73', label = 'Fraction'),
                                               Item(name = 'enr73', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele73 == "3-Li" or ele73 == "5-B" or ele73 == "92-U"'),
                                               Item(name = 'Li6_73', label = 'Li-6 fraction',
                                                    visible_when = 'ele73 == "3-Li" and enr73 == "Enriched"'),
                                               Item(name = 'Li7_73', label = 'Li-7 fraction',
                                                    visible_when = 'ele73 == "3-Li" and enr73 == "Enriched"'),
                                               Item(name = 'B10_73', label = 'B-10 fraction',
                                                    visible_when = 'ele73 == "5-B" and enr73 == "Enriched"'),
                                               Item(name = 'B11_73', label = 'B-11 fraction',
                                                    visible_when = 'ele73 == "5-B" and enr73 == "Enriched"'),
                                               Item(name = 'U234_73', label = 'U-234 fraction',
                                                    visible_when = 'ele73 == "92-U" and enr73 == "Enriched"'),
                                               Item(name = 'U235_73', label = 'U-235 fraction',
                                                    visible_when = 'ele73 == "92-U" and enr73 == "Enriched"'),
                                               Item(name = 'U238_73', label = 'U-238 fraction',
                                                    visible_when = 'ele73 == "92-U" and enr73 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 72'
                                             ),
                                        Group(
                                               Item(name = 'ele74', label = 'Element', width = -100),
                                               Item(name = 'frac74', label = 'Fraction'),
                                               Item(name = 'enr74', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele74 == "3-Li" or ele74 == "5-B" or ele74 == "92-U"'),
                                               Item(name = 'Li6_74', label = 'Li-6 fraction',
                                                    visible_when = 'ele74 == "3-Li" and enr74 == "Enriched"'),
                                               Item(name = 'Li7_74', label = 'Li-7 fraction',
                                                    visible_when = 'ele74 == "3-Li" and enr74 == "Enriched"'),
                                               Item(name = 'B10_74', label = 'B-10 fraction',
                                                    visible_when = 'ele74 == "5-B" and enr74 == "Enriched"'),
                                               Item(name = 'B11_74', label = 'B-11 fraction',
                                                    visible_when = 'ele74 == "5-B" and enr74 == "Enriched"'),
                                               Item(name = 'U234_74', label = 'U-234 fraction',
                                                    visible_when = 'ele74 == "92-U" and enr74 == "Enriched"'),
                                               Item(name = 'U235_74', label = 'U-235 fraction',
                                                    visible_when = 'ele74 == "92-U" and enr74 == "Enriched"'),
                                               Item(name = 'U238_74', label = 'U-238 fraction',
                                                    visible_when = 'ele74 == "92-U" and enr74 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 73'
                                             ),
                                        Group(
                                               Item(name = 'ele75', label = 'Element', width = -100),
                                               Item(name = 'frac75', label = 'Fraction'),
                                               Item(name = 'enr75', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele75 == "3-Li" or ele75 == "5-B" or ele75 == "92-U"'),
                                               Item(name = 'Li6_75', label = 'Li-6 fraction',
                                                    visible_when = 'ele75 == "3-Li" and enr75 == "Enriched"'),
                                               Item(name = 'Li7_75', label = 'Li-7 fraction',
                                                    visible_when = 'ele75 == "3-Li" and enr75 == "Enriched"'),
                                               Item(name = 'B10_75', label = 'B-10 fraction',
                                                    visible_when = 'ele75 == "5-B" and enr75 == "Enriched"'),
                                               Item(name = 'B11_75', label = 'B-11 fraction',
                                                    visible_when = 'ele75 == "5-B" and enr75 == "Enriched"'),
                                               Item(name = 'U234_75', label = 'U-234 fraction',
                                                    visible_when = 'ele75 == "92-U" and enr75 == "Enriched"'),
                                               Item(name = 'U235_75', label = 'U-235 fraction',
                                                    visible_when = 'ele75 == "92-U" and enr75 == "Enriched"'),
                                               Item(name = 'U238_75', label = 'U-238 fraction',
                                                    visible_when = 'ele75 == "92-U" and enr75 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 74'
                                             ),
                                        Group(
                                               Item(name = 'ele76', label = 'Element', width = -100),
                                               Item(name = 'frac76', label = 'Fraction'),
                                               Item(name = 'enr76', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele76 == "3-Li" or ele76 == "5-B" or ele76 == "92-U"'),
                                               Item(name = 'Li6_76', label = 'Li-6 fraction',
                                                    visible_when = 'ele76 == "3-Li" and enr76 == "Enriched"'),
                                               Item(name = 'Li7_76', label = 'Li-7 fraction',
                                                    visible_when = 'ele76 == "3-Li" and enr76 == "Enriched"'),
                                               Item(name = 'B10_76', label = 'B-10 fraction',
                                                    visible_when = 'ele76 == "5-B" and enr76 == "Enriched"'),
                                               Item(name = 'B11_76', label = 'B-11 fraction',
                                                    visible_when = 'ele76 == "5-B" and enr76 == "Enriched"'),
                                               Item(name = 'U234_76', label = 'U-234 fraction',
                                                    visible_when = 'ele76 == "92-U" and enr76 == "Enriched"'),
                                               Item(name = 'U235_76', label = 'U-235 fraction',
                                                    visible_when = 'ele76 == "92-U" and enr76 == "Enriched"'),
                                               Item(name = 'U238_76', label = 'U-238 fraction',
                                                    visible_when = 'ele76 == "92-U" and enr76 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 75'
                                             ),
                                        Group(
                                               Item(name = 'ele77', label = 'Element', width = -100),
                                               Item(name = 'frac77', label = 'Fraction'),
                                               Item(name = 'enr77', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele77 == "3-Li" or ele77 == "5-B" or ele77 == "92-U"'),
                                               Item(name = 'Li6_77', label = 'Li-6 fraction',
                                                    visible_when = 'ele77 == "3-Li" and enr77 == "Enriched"'),
                                               Item(name = 'Li7_77', label = 'Li-7 fraction',
                                                    visible_when = 'ele77 == "3-Li" and enr77 == "Enriched"'),
                                               Item(name = 'B10_77', label = 'B-10 fraction',
                                                    visible_when = 'ele77 == "5-B" and enr77 == "Enriched"'),
                                               Item(name = 'B11_77', label = 'B-11 fraction',
                                                    visible_when = 'ele77 == "5-B" and enr77 == "Enriched"'),
                                               Item(name = 'U234_77', label = 'U-234 fraction',
                                                    visible_when = 'ele77 == "92-U" and enr77 == "Enriched"'),
                                               Item(name = 'U235_77', label = 'U-235 fraction',
                                                    visible_when = 'ele77 == "92-U" and enr77 == "Enriched"'),
                                               Item(name = 'U238_77', label = 'U-238 fraction',
                                                    visible_when = 'ele77 == "92-U" and enr77 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 76'
                                             ),
                                        Group(
                                               Item(name = 'ele78', label = 'Element', width = -100),
                                               Item(name = 'frac78', label = 'Fraction'),
                                               Item(name = 'enr78', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele78 == "3-Li" or ele78 == "5-B" or ele78 == "92-U"'),
                                               Item(name = 'Li6_78', label = 'Li-6 fraction',
                                                    visible_when = 'ele78 == "3-Li" and enr78 == "Enriched"'),
                                               Item(name = 'Li7_78', label = 'Li-7 fraction',
                                                    visible_when = 'ele78 == "3-Li" and enr78 == "Enriched"'),
                                               Item(name = 'B10_78', label = 'B-10 fraction',
                                                    visible_when = 'ele78 == "5-B" and enr78 == "Enriched"'),
                                               Item(name = 'B11_78', label = 'B-11 fraction',
                                                    visible_when = 'ele78 == "5-B" and enr78 == "Enriched"'),
                                               Item(name = 'U234_78', label = 'U-234 fraction',
                                                    visible_when = 'ele78 == "92-U" and enr78 == "Enriched"'),
                                               Item(name = 'U235_78', label = 'U-235 fraction',
                                                    visible_when = 'ele78 == "92-U" and enr78 == "Enriched"'),
                                               Item(name = 'U238_78', label = 'U-238 fraction',
                                                    visible_when = 'ele78 == "92-U" and enr78 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 77'
                                             ),
                                        Group(
                                               Item(name = 'ele79', label = 'Element', width = -100),
                                               Item(name = 'frac79', label = 'Fraction'),
                                               Item(name = 'enr79', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele79 == "3-Li" or ele79 == "5-B" or ele79 == "92-U"'),
                                               Item(name = 'Li6_79', label = 'Li-6 fraction',
                                                    visible_when = 'ele79 == "3-Li" and enr79 == "Enriched"'),
                                               Item(name = 'Li7_79', label = 'Li-7 fraction',
                                                    visible_when = 'ele79 == "3-Li" and enr79 == "Enriched"'),
                                               Item(name = 'B10_79', label = 'B-10 fraction',
                                                    visible_when = 'ele79 == "5-B" and enr79 == "Enriched"'),
                                               Item(name = 'B11_79', label = 'B-11 fraction',
                                                    visible_when = 'ele79 == "5-B" and enr79 == "Enriched"'),
                                               Item(name = 'U234_79', label = 'U-234 fraction',
                                                    visible_when = 'ele79 == "92-U" and enr79 == "Enriched"'),
                                               Item(name = 'U235_79', label = 'U-235 fraction',
                                                    visible_when = 'ele79 == "92-U" and enr79 == "Enriched"'),
                                               Item(name = 'U238_79', label = 'U-238 fraction',
                                                    visible_when = 'ele79 == "92-U" and enr79 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 78'
                                             ),
                                        Group(
                                               Item(name = 'ele80', label = 'Element', width = -100),
                                               Item(name = 'frac80', label = 'Fraction'),
                                               Item(name = 'enr80', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele80 == "3-Li" or ele80 == "5-B" or ele80 == "92-U"'),
                                               Item(name = 'Li6_80', label = 'Li-6 fraction',
                                                    visible_when = 'ele80 == "3-Li" and enr80 == "Enriched"'),
                                               Item(name = 'Li7_80', label = 'Li-7 fraction',
                                                    visible_when = 'ele80 == "3-Li" and enr80 == "Enriched"'),
                                               Item(name = 'B10_80', label = 'B-10 fraction',
                                                    visible_when = 'ele80 == "5-B" and enr80 == "Enriched"'),
                                               Item(name = 'B11_80', label = 'B-11 fraction',
                                                    visible_when = 'ele80 == "5-B" and enr80 == "Enriched"'),
                                               Item(name = 'U234_80', label = 'U-234 fraction',
                                                    visible_when = 'ele80 == "92-U" and enr80 == "Enriched"'),
                                               Item(name = 'U235_80', label = 'U-235 fraction',
                                                    visible_when = 'ele80 == "92-U" and enr80 == "Enriched"'),
                                               Item(name = 'U238_80', label = 'U-238 fraction',
                                                    visible_when = 'ele80 == "92-U" and enr80 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 79'
                                             ),
                                        Group(
                                               Item(name = 'ele81', label = 'Element', width = -100),
                                               Item(name = 'frac81', label = 'Fraction'),
                                               Item(name = 'enr81', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele81 == "3-Li" or ele81 == "5-B" or ele81 == "92-U"'),
                                               Item(name = 'Li6_81', label = 'Li-6 fraction',
                                                    visible_when = 'ele81 == "3-Li" and enr81 == "Enriched"'),
                                               Item(name = 'Li7_81', label = 'Li-7 fraction',
                                                    visible_when = 'ele81 == "3-Li" and enr81 == "Enriched"'),
                                               Item(name = 'B10_81', label = 'B-10 fraction',
                                                    visible_when = 'ele81 == "5-B" and enr81 == "Enriched"'),
                                               Item(name = 'B11_81', label = 'B-11 fraction',
                                                    visible_when = 'ele81 == "5-B" and enr81 == "Enriched"'),
                                               Item(name = 'U234_81', label = 'U-234 fraction',
                                                    visible_when = 'ele81 == "92-U" and enr81 == "Enriched"'),
                                               Item(name = 'U235_81', label = 'U-235 fraction',
                                                    visible_when = 'ele81 == "92-U" and enr81 == "Enriched"'),
                                               Item(name = 'U238_81', label = 'U-238 fraction',
                                                    visible_when = 'ele81 == "92-U" and enr81 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 80'
                                             ),
                                        Group(
                                               Item(name = 'ele82', label = 'Element', width = -100),
                                               Item(name = 'frac82', label = 'Fraction'),
                                               Item(name = 'enr82', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele82 == "3-Li" or ele82 == "5-B" or ele82 == "92-U"'),
                                               Item(name = 'Li6_82', label = 'Li-6 fraction',
                                                    visible_when = 'ele82 == "3-Li" and enr82 == "Enriched"'),
                                               Item(name = 'Li7_82', label = 'Li-7 fraction',
                                                    visible_when = 'ele82 == "3-Li" and enr82 == "Enriched"'),
                                               Item(name = 'B10_82', label = 'B-10 fraction',
                                                    visible_when = 'ele82 == "5-B" and enr82 == "Enriched"'),
                                               Item(name = 'B11_82', label = 'B-11 fraction',
                                                    visible_when = 'ele82 == "5-B" and enr82 == "Enriched"'),
                                               Item(name = 'U234_82', label = 'U-234 fraction',
                                                    visible_when = 'ele82 == "92-U" and enr82 == "Enriched"'),
                                               Item(name = 'U235_82', label = 'U-235 fraction',
                                                    visible_when = 'ele82 == "92-U" and enr82 == "Enriched"'),
                                               Item(name = 'U238_82', label = 'U-238 fraction',
                                                    visible_when = 'ele82 == "92-U" and enr82 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 81'
                                             ),
                                        Group(
                                               Item(name = 'ele83', label = 'Element', width = -100),
                                               Item(name = 'frac83', label = 'Fraction'),
                                               Item(name = 'enr83', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele83 == "3-Li" or ele83 == "5-B" or ele83 == "92-U"'),
                                               Item(name = 'Li6_83', label = 'Li-6 fraction',
                                                    visible_when = 'ele83 == "3-Li" and enr83 == "Enriched"'),
                                               Item(name = 'Li7_83', label = 'Li-7 fraction',
                                                    visible_when = 'ele83 == "3-Li" and enr83 == "Enriched"'),
                                               Item(name = 'B10_83', label = 'B-10 fraction',
                                                    visible_when = 'ele83 == "5-B" and enr83 == "Enriched"'),
                                               Item(name = 'B11_83', label = 'B-11 fraction',
                                                    visible_when = 'ele83 == "5-B" and enr83 == "Enriched"'),
                                               Item(name = 'U234_83', label = 'U-234 fraction',
                                                    visible_when = 'ele83 == "92-U" and enr83 == "Enriched"'),
                                               Item(name = 'U235_83', label = 'U-235 fraction',
                                                    visible_when = 'ele83 == "92-U" and enr83 == "Enriched"'),
                                               Item(name = 'U238_83', label = 'U-238 fraction',
                                                    visible_when = 'ele83 == "92-U" and enr83 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 82'
                                             ),
                                        Group(
                                               Item(name = 'ele84', label = 'Element', width = -100),
                                               Item(name = 'frac84', label = 'Fraction'),
                                               Item(name = 'enr84', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele84 == "3-Li" or ele84 == "5-B" or ele84 == "92-U"'),
                                               Item(name = 'Li6_84', label = 'Li-6 fraction',
                                                    visible_when = 'ele84 == "3-Li" and enr84 == "Enriched"'),
                                               Item(name = 'Li7_84', label = 'Li-7 fraction',
                                                    visible_when = 'ele84 == "3-Li" and enr84 == "Enriched"'),
                                               Item(name = 'B10_84', label = 'B-10 fraction',
                                                    visible_when = 'ele84 == "5-B" and enr84 == "Enriched"'),
                                               Item(name = 'B11_84', label = 'B-11 fraction',
                                                    visible_when = 'ele84 == "5-B" and enr84 == "Enriched"'),
                                               Item(name = 'U234_84', label = 'U-234 fraction',
                                                    visible_when = 'ele84 == "92-U" and enr84 == "Enriched"'),
                                               Item(name = 'U235_84', label = 'U-235 fraction',
                                                    visible_when = 'ele84 == "92-U" and enr84 == "Enriched"'),
                                               Item(name = 'U238_84', label = 'U-238 fraction',
                                                    visible_when = 'ele84 == "92-U" and enr84 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 83'
                                             ),
                                        Group(
                                               Item(name = 'ele85', label = 'Element', width = -100),
                                               Item(name = 'frac85', label = 'Fraction'),
                                               Item(name = 'enr85', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele85 == "3-Li" or ele85 == "5-B" or ele85 == "92-U"'),
                                               Item(name = 'Li6_85', label = 'Li-6 fraction',
                                                    visible_when = 'ele85 == "3-Li" and enr85 == "Enriched"'),
                                               Item(name = 'Li7_85', label = 'Li-7 fraction',
                                                    visible_when = 'ele85 == "3-Li" and enr85 == "Enriched"'),
                                               Item(name = 'B10_85', label = 'B-10 fraction',
                                                    visible_when = 'ele85 == "5-B" and enr85 == "Enriched"'),
                                               Item(name = 'B11_85', label = 'B-11 fraction',
                                                    visible_when = 'ele85 == "5-B" and enr85 == "Enriched"'),
                                               Item(name = 'U234_85', label = 'U-234 fraction',
                                                    visible_when = 'ele85 == "92-U" and enr85 == "Enriched"'),
                                               Item(name = 'U235_85', label = 'U-235 fraction',
                                                    visible_when = 'ele85 == "92-U" and enr85 == "Enriched"'),
                                               Item(name = 'U238_85', label = 'U-238 fraction',
                                                    visible_when = 'ele85 == "92-U" and enr85 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 84'
                                             ),
                                        Group(
                                               Item(name = 'ele86', label = 'Element', width = -100),
                                               Item(name = 'frac86', label = 'Fraction'),
                                               Item(name = 'enr86', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele86 == "3-Li" or ele86 == "5-B" or ele86 == "92-U"'),
                                               Item(name = 'Li6_86', label = 'Li-6 fraction',
                                                    visible_when = 'ele86 == "3-Li" and enr86 == "Enriched"'),
                                               Item(name = 'Li7_86', label = 'Li-7 fraction',
                                                    visible_when = 'ele86 == "3-Li" and enr86 == "Enriched"'),
                                               Item(name = 'B10_86', label = 'B-10 fraction',
                                                    visible_when = 'ele86 == "5-B" and enr86 == "Enriched"'),
                                               Item(name = 'B11_86', label = 'B-11 fraction',
                                                    visible_when = 'ele86 == "5-B" and enr86 == "Enriched"'),
                                               Item(name = 'U234_86', label = 'U-234 fraction',
                                                    visible_when = 'ele86 == "92-U" and enr86 == "Enriched"'),
                                               Item(name = 'U235_86', label = 'U-235 fraction',
                                                    visible_when = 'ele86 == "92-U" and enr86 == "Enriched"'),
                                               Item(name = 'U238_86', label = 'U-238 fraction',
                                                    visible_when = 'ele86 == "92-U" and enr86 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 85'
                                             ),
                                        Group(
                                               Item(name = 'ele87', label = 'Element', width = -100),
                                               Item(name = 'frac87', label = 'Fraction'),
                                               Item(name = 'enr87', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele87 == "3-Li" or ele87 == "5-B" or ele87 == "92-U"'),
                                               Item(name = 'Li6_87', label = 'Li-6 fraction',
                                                    visible_when = 'ele87 == "3-Li" and enr87 == "Enriched"'),
                                               Item(name = 'Li7_87', label = 'Li-7 fraction',
                                                    visible_when = 'ele87 == "3-Li" and enr87 == "Enriched"'),
                                               Item(name = 'B10_87', label = 'B-10 fraction',
                                                    visible_when = 'ele87 == "5-B" and enr87 == "Enriched"'),
                                               Item(name = 'B11_87', label = 'B-11 fraction',
                                                    visible_when = 'ele87 == "5-B" and enr87 == "Enriched"'),
                                               Item(name = 'U234_87', label = 'U-234 fraction',
                                                    visible_when = 'ele87 == "92-U" and enr87 == "Enriched"'),
                                               Item(name = 'U235_87', label = 'U-235 fraction',
                                                    visible_when = 'ele87 == "92-U" and enr87 == "Enriched"'),
                                               Item(name = 'U238_87', label = 'U-238 fraction',
                                                    visible_when = 'ele87 == "92-U" and enr87 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 86'
                                             ),
                                        Group(
                                               Item(name = 'ele88', label = 'Element', width = -100),
                                               Item(name = 'frac88', label = 'Fraction'),
                                               Item(name = 'enr88', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele88 == "3-Li" or ele88 == "5-B" or ele88 == "92-U"'),
                                               Item(name = 'Li6_88', label = 'Li-6 fraction',
                                                    visible_when = 'ele88 == "3-Li" and enr88 == "Enriched"'),
                                               Item(name = 'Li7_88', label = 'Li-7 fraction',
                                                    visible_when = 'ele88 == "3-Li" and enr88 == "Enriched"'),
                                               Item(name = 'B10_88', label = 'B-10 fraction',
                                                    visible_when = 'ele88 == "5-B" and enr88 == "Enriched"'),
                                               Item(name = 'B11_88', label = 'B-11 fraction',
                                                    visible_when = 'ele88 == "5-B" and enr88 == "Enriched"'),
                                               Item(name = 'U234_88', label = 'U-234 fraction',
                                                    visible_when = 'ele88 == "92-U" and enr88 == "Enriched"'),
                                               Item(name = 'U235_88', label = 'U-235 fraction',
                                                    visible_when = 'ele88 == "92-U" and enr88 == "Enriched"'),
                                               Item(name = 'U238_88', label = 'U-238 fraction',
                                                    visible_when = 'ele88 == "92-U" and enr88 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 87'
                                             ),
                                        Group(
                                               Item(name = 'ele89', label = 'Element', width = -100),
                                               Item(name = 'frac89', label = 'Fraction'),
                                               Item(name = 'enr89', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele89 == "3-Li" or ele89 == "5-B" or ele89 == "92-U"'),
                                               Item(name = 'Li6_89', label = 'Li-6 fraction',
                                                    visible_when = 'ele89 == "3-Li" and enr89 == "Enriched"'),
                                               Item(name = 'Li7_89', label = 'Li-7 fraction',
                                                    visible_when = 'ele89 == "3-Li" and enr89 == "Enriched"'),
                                               Item(name = 'B10_89', label = 'B-10 fraction',
                                                    visible_when = 'ele89 == "5-B" and enr89 == "Enriched"'),
                                               Item(name = 'B11_89', label = 'B-11 fraction',
                                                    visible_when = 'ele89 == "5-B" and enr89 == "Enriched"'),
                                               Item(name = 'U234_89', label = 'U-234 fraction',
                                                    visible_when = 'ele89 == "92-U" and enr89 == "Enriched"'),
                                               Item(name = 'U235_89', label = 'U-235 fraction',
                                                    visible_when = 'ele89 == "92-U" and enr89 == "Enriched"'),
                                               Item(name = 'U238_89', label = 'U-238 fraction',
                                                    visible_when = 'ele89 == "92-U" and enr89 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 88'
                                             ),
                                        Group(
                                               Item(name = 'ele90', label = 'Element', width = -100),
                                               Item(name = 'frac90', label = 'Fraction'),
                                               Item(name = 'enr90', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele90 == "3-Li" or ele90 == "5-B" or ele90 == "92-U"'),
                                               Item(name = 'Li6_90', label = 'Li-6 fraction',
                                                    visible_when = 'ele90 == "3-Li" and enr90 == "Enriched"'),
                                               Item(name = 'Li7_90', label = 'Li-7 fraction',
                                                    visible_when = 'ele90 == "3-Li" and enr90 == "Enriched"'),
                                               Item(name = 'B10_90', label = 'B-10 fraction',
                                                    visible_when = 'ele90 == "5-B" and enr90 == "Enriched"'),
                                               Item(name = 'B11_90', label = 'B-11 fraction',
                                                    visible_when = 'ele90 == "5-B" and enr90 == "Enriched"'),
                                               Item(name = 'U234_90', label = 'U-234 fraction',
                                                    visible_when = 'ele90 == "92-U" and enr90 == "Enriched"'),
                                               Item(name = 'U235_90', label = 'U-235 fraction',
                                                    visible_when = 'ele90 == "92-U" and enr90 == "Enriched"'),
                                               Item(name = 'U238_90', label = 'U-238 fraction',
                                                    visible_when = 'ele90 == "92-U" and enr90 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 89'
                                             ),
                                        Group(
                                               Item(name = 'ele91', label = 'Element', width = -100),
                                               Item(name = 'frac91', label = 'Fraction'),
                                               Item(name = 'enr91', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele91 == "3-Li" or ele91 == "5-B" or ele91 == "92-U"'),
                                               Item(name = 'Li6_91', label = 'Li-6 fraction',
                                                    visible_when = 'ele91 == "3-Li" and enr91 == "Enriched"'),
                                               Item(name = 'Li7_91', label = 'Li-7 fraction',
                                                    visible_when = 'ele91 == "3-Li" and enr91 == "Enriched"'),
                                               Item(name = 'B10_91', label = 'B-10 fraction',
                                                    visible_when = 'ele91 == "5-B" and enr91 == "Enriched"'),
                                               Item(name = 'B11_91', label = 'B-11 fraction',
                                                    visible_when = 'ele91 == "5-B" and enr91 == "Enriched"'),
                                               Item(name = 'U234_91', label = 'U-234 fraction',
                                                    visible_when = 'ele91 == "92-U" and enr91 == "Enriched"'),
                                               Item(name = 'U235_91', label = 'U-235 fraction',
                                                    visible_when = 'ele91 == "92-U" and enr91 == "Enriched"'),
                                               Item(name = 'U238_91', label = 'U-238 fraction',
                                                    visible_when = 'ele91 == "92-U" and enr91 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 90'
                                             ),
                                        Group(
                                               Item(name = 'ele92', label = 'Element', width = -100),
                                               Item(name = 'frac92', label = 'Fraction'),
                                               Item(name = 'enr92', label = 'Isotope fractions', width = -200,
                                                    visible_when = 'ele92 == "3-Li" or ele92 == "5-B" or ele92 == "92-U"'),
                                               Item(name = 'Li6_92', label = 'Li-6 fraction',
                                                    visible_when = 'ele92 == "3-Li" and enr92 == "Enriched"'),
                                               Item(name = 'Li7_92', label = 'Li-7 fraction',
                                                    visible_when = 'ele92 == "3-Li" and enr92 == "Enriched"'),
                                               Item(name = 'B10_92', label = 'B-10 fraction',
                                                    visible_when = 'ele92 == "5-B" and enr92 == "Enriched"'),
                                               Item(name = 'B11_92', label = 'B-11 fraction',
                                                    visible_when = 'ele92 == "5-B" and enr92 == "Enriched"'),
                                               Item(name = 'U234_92', label = 'U-234 fraction',
                                                    visible_when = 'ele92 == "92-U" and enr92 == "Enriched"'),
                                               Item(name = 'U235_92', label = 'U-235 fraction',
                                                    visible_when = 'ele92 == "92-U" and enr92 == "Enriched"'),
                                               Item(name = 'U238_92', label = 'U-238 fraction',
                                                    visible_when = 'ele92 == "92-U" and enr92 == "Enriched"'),
                                               spring,
                                               orientation = 'horizontal', visible_when = 'num_ele > 91'
                                             ),
                                        orientation = 'vertical'
                                      ), 
                            orientation = 'vertical', label = 'Material Description'
                          ),
                     Group(
                            UItem('save_file', show_label = False,
                                  tooltip = "Clicking this button will save the state of all fields in the interface in a .pkl file in the "\
                                            "\"Input file location\" field specified on the \"Setup\" tab."),
                            UItem('run_matmcnp', label = 'Run MatMCNP',
                                  tooltip = "Clicking this button will write the MatMCNP input file and run MatMCNP in the same directory. "\
                                             "The input file will be saved as <Input file name>.inp in the \"Input file location\" field specified on the \"Setup\" tab."),
                            orientation = 'vertical', label = 'Save/Run'
                          ),
                     layout = 'tabbed'
                   ),
              scrollable = True, resizable = True, width = 1500, height = 800, title = 'MatMCNP', handler=ClosingHandler()
            ) 
    
if __name__ == '__main__':
    file_writer = InputFile()
    tmpfile = open('tmpfile', 'r')
    check = tmpfile.read().splitlines()
    if check[0] == 'True':
        file_writer.configure_traits(view=view1)
    else: 
        line = check[1]
        parts = line.split('/')
        readfile = open(line,'rb')
        file_writer = pickle.load(readfile)
        file_writer.configure_traits(view=view1)



