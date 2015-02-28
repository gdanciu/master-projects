/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package kripke;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.LinkedHashSet;
import java.util.Scanner;
import java.util.Stack;
import java.util.StringTokenizer;
import java.util.Vector;

class Logger {

    static String nec = ".", pos = ",", impl = ";", iff = "-";
    void log(String s) {
        System.out.print( s + "\n");
    }

    void disp(String s) {
        System.out.print( s + "\n");
    }
}

class Relation {
    String a, b;

    Relation() {}
    Relation(String a, String b) {
        this.a = a;
        this.b = b;
    }

    boolean equals(Relation r) {
        if (r.a.equals(a) && r.b.equals(b))
            return true;
        return false;
    }

}

class Model extends Logger{
    Set<String> Worlds;
    Set<Relation> Relations;
    Map<String, Set<String>> Truths;

    Model() {
        Worlds = new LinkedHashSet<String>();
        Relations = new LinkedHashSet<Relation>();
        Truths = new HashMap<String, Set<String>>();
    }
    String show() {
        String ret = "", separator = "";
        ret = "  Worlds = {";
        for ( String w: Worlds) {
            ret += separator + w;
            separator = ", ";
        }
        ret += "}\n  Relation = {";
        separator = "";
        for ( Relation r: Relations ) {
            ret += separator + "(" + r.a + ", " + r.b + ")";
            separator = ", ";
        }
        ret += "}";
        for (String w: Worlds) {
            ret += "\n  True(" + w + ") = {";
            separator = "";
            for (String t:Truths.get(w)) {
                ret += separator + t;
                separator = ", ";
            }
            ret += "}";
        }
        return ret;
    }

    int addWorlds(String[] worlds) {
        for (int i = 0; i < worlds.length; i++){
            if (Worlds.contains(worlds[i]))
                log("World " + worlds[i] + " already exists");
            else {
                Worlds.add(worlds[i]);
                Truths.put(worlds[i], new LinkedHashSet<String>());
            }
        }
        return 0;
    }

    int removeWorlds(String[] worlds) {
        for (int i = 0; i < worlds.length; i++) {
            if (Worlds.contains(worlds[i])) {
                for (Relation r: Relations) {
                    if (r.a.equals(worlds[i]) || r.b.equals(worlds[i]))
                        Relations.remove(r);
                }
                Worlds.remove(worlds[i]);
                Truths.remove(worlds[i]);
            }
            else
                log("World " + worlds[i] + " not found");
        }
        return 0;
    }

    int renameWorld(String oldW, String newW) {
        if (Worlds.contains(oldW)) {
            Worlds.remove(oldW);
            Worlds.add(newW);
            Set newRelations = new LinkedHashSet<Relation>();
            Relation old = new Relation();
            while (old != null) {
                old = null;
                for (Relation r: Relations) {
                    if (r.a.equals(oldW) || r.b.equals(oldW)) {
                        Relation newR = new Relation();
                        newR.a = r.a.equals(oldW) ? newW : r.a;
                        newR.b = r.b.equals(oldW) ? newW : r.b;
                        newRelations.add(newR);
                        old = r;
                        break;
                    }
                }
                Relations.remove(old);
            }

            Relations.addAll(newRelations);
            if (Truths.containsKey(newW)) {
                Truths.get(newW).add(oldW);
            }
            else {
                Truths.put(newW, Truths.get(oldW));
            }
            Truths.remove(oldW);
        }
        else
            log("World " + oldW + " not found");
        return 0;
    }

    int addRelations(String[] worlds) {
        for (int i = 0; i < worlds.length /2; i++) {
            if (!Worlds.contains(worlds[2 * i])) {
                log( "World " + worlds[2*i] + " not found");
            } else if (!Worlds.contains(worlds[2 * i + 1])) {
                log( "World " + worlds[2*i + 1] + " not found");
            } else {
                Relation r = new Relation();
                r.a = worlds[2*i];
                r.b = worlds[2*i + 1];
                if (!Relations.add(r))
                    log ("Pair (" + worlds[2*i] + ", " + worlds[2*i + 1] +
                            ") already exists.");
            }
        }
        return 0;
    }

    int removeRelations(String[] worlds) {
        for (int i = 0; i < worlds.length /2; i++) {
            Relation rem = null;
            for (Relation r: Relations) {
                if (r.a.equals(worlds[2*i]) && r.b.equals(worlds[2*i+1]))                     
                    rem = r;
            }
            if (rem == null)
                log ("Pair (" + worlds[2*i] + "," + worlds[2*i+1]+") not found");
            else
                Relations.remove(rem);
        }
        return 0;
    }

    void clearRelations() {
        Relations.clear();
    }

    void addTrue(String[] props, String[] worlds) {
        int i, j;
        for (i = 0; i < worlds.length; i++) {
            if ( !Worlds.contains(worlds[i]) ) {
                log("World " + worlds[i] + " not found.");
            }
            else {
                for (j = 0; j < props.length; j++)
                    if(!Truths.get(worlds[i]).add(props[j]))
                        log("Proposition " + props[j] +
                                " already true at world " + worlds[i] + ".");
            }
        }
    }

    void removeTrue(String[] props, String[] worlds) {
        int i, j;
        for (i = 0; i < worlds.length; i++) {
            if ( !Worlds.contains(worlds[i]) ) {
                log("World " + worlds[i] + " not found.");
            }
            else {
                for (j = 0; j < props.length; j++)
                    if (!Truths.get(worlds[i]).remove(props[j]))
                        log ("Proposition " + props[j] + " was not true in " +
                                worlds[i]);
            }
        }
    }

    void removeProps(String[] props) {
        for (String w: Worlds) {
            for (int i = 0; i < props.length; i++)
                Truths.get(w).remove(props[i]);
        }
    }

    void renameProp( String oldP, String newP ) {
        for (String w: Worlds) {
            Truths.get(w).remove(oldP);
            Truths.get(w).add(newP);
        }
    }

    void clearTruth() {
        for (String w:Truths.keySet())
            Truths.get(w).clear();
    }

    Set<String> nextWorlds(String world) {
        Set<String> ret = new LinkedHashSet<String>();
        for (Relation r : Relations) {
            if (r.a.equals(world)) {
                ret.add(r.b);
            }
        }
        return ret;
    }
}


class KNode extends Logger {
    String Formula;
    Vector<KNode> Children;
    String Info, Eval, World;
    Vector<String> Why;
    Model m;
    KNode necOrPosRouteNode; // if nec is false, then why will go only on this
                //node, otherwise on all children

    KNode() {
        Children = new Vector<KNode>();
        Formula="";
        necOrPosRouteNode = null;
    }

    static void buildFromStack(Stack<KNode> prStack, Stack<String> opStack,
            Model m, String w) {
        while (!opStack.isEmpty()) {
            String op = opStack.pop();
            KNode n = new KNode();
            n.Info = op;
            n.World = w;
            n.m = m;
            KNode child1 = prStack.pop();
            if (!op.equals("~")) {
                KNode child2 = prStack.pop();
                n.Children.add(child2);
                if (0 < precedence (child2.Info)  &&
                    precedence (child2.Info) < precedence(op))
                    n.Formula = "(" + child2.Formula +")";
                else
                    n.Formula = child2.Formula;
                n.Formula += op;
                if (0 < precedence (child1.Info)  &&
                    precedence (child1.Info) < precedence(op))
                    n.Formula += "(" + child1.Formula +")";
                else
                    n.Formula += child1.Formula;
            }
            else {
                if (0 < precedence (child1.Info))
                    n.Formula = "~(" + child1.Formula +")";
                else
                    n.Formula = "~" + child1.Formula;
            }
            n.Children.add(child1);
            prStack.push(n);
            //System.out.print(prStack.get(0).Formula);
        }
    }

    static String evalProp(String prop, Model m, String w) {
        if (m.Truths.get(w).contains(prop))
            return "T";
        return "F";
    }

    //Integer pos = 0;
    static String convert(String formula) {
        formula.replaceAll("<->", iff);
        formula.replaceAll("``", nec);
        return formula.replaceAll("<>", pos).replaceAll("->", impl).replaceAll(" ", "");
    }

    static int precedence (String operator) {
        if (operator.equals(iff) ) return 1;
        if (operator.equals(impl)) return 2;
        if (operator.equals("|")) return 3;
        if (operator.equals("&")) return 4;
        if (operator.equals(nec) || operator.equals(pos) || operator.equals("~"))
            return 5;
        if (operator.equals("(") || operator.equals(")"))
            return -1;
        return 0;
    }


    static String necOrPosParam(StringTokenizer form) {
        int paran = 0;
        String ret = "", next = form.nextToken();

        while (paran > 0 || precedence(next) == 5 || next.equals("(")) {
            ret += next;
            if (next.equals("("))
                paran++;
            if (next.equals(")"))
                paran--;
            next = form.nextToken();
        }
        ret += next;
        return ret;
    }

    static KNode buildTree(StringTokenizer form, Model m, String world) {
        //returns the root

        Stack<KNode> prStack = new Stack<KNode>();
        Stack<String> opStack = new Stack<String>();
        while (form.hasMoreTokens()) {
            String next = form.nextToken();
            if (next.equals("(")) {
                prStack.push(buildTree(form, m, world));
            }
            else if (next.equals(")")) {
                buildFromStack(prStack, opStack, m, world);
                return prStack.pop();
            }
            else if (precedence(next) == 0) {
                KNode n = new KNode();
                n.Info = next;
                n.Formula = next;
                n.World = world;
                n.m = m;
                prStack.push(n);
            }
            else if (next.equals(nec) || next.equals(pos)){
                KNode n = new KNode();
                n.World = world;
                n.m = m;
                n.Formula = next + necOrPosParam(form);
                n.Info = next;
                StringTokenizer st;
                for (String w : m.nextWorlds(world)) {
                    st = new StringTokenizer(n.Formula,
                            nec + pos + impl + iff + "|&~()", true);
                    n.Children.add(buildTree(st, m, w));
                }
                prStack.push(n);
            }
            else if (opStack.isEmpty() || precedence(next) >= precedence(opStack.get(opStack.size()-1))) {
                opStack.push(next);
            }
            else {
                buildFromStack(prStack, opStack, m, world);
                opStack.push(next);
            }
        }
        buildFromStack(prStack, opStack, m, world);
        return prStack.pop();
    }

    static KNode buildTreeFinal(String formula, Model m, String world) {
        formula = convert(formula);
        StringTokenizer form = new StringTokenizer(formula,
                nec + pos + impl + iff + "|&~()", true);
        return buildTree(form, m, world);
    }

    static String l(String tf) {
        return tf.equals("T") ? "TRUE" : "FALSE";
    }

    static String eval(KNode root) {
        Vector<String> childrenEval = new Vector<String>();
        root.Why = new Vector<String>();

        if (root.Info.equals("T") || root.Info.equals("F")) {
            root.Eval = root.Info;
            root.Why.add(root.Info +" is "+ l(root.Eval)+ " at world " + root.World);
        }
        else if (precedence(root.Info) == 0) {
            root.Eval = evalProp(root.Info, root.m, root.World);
            root.Why.add(root.Info + (root.Eval.equals("T") ? " " : " not ")
                    + "in True(" + root.World + ")");
        }
        else if (root.Info.equals("~")) {
            childrenEval.add(eval(root.Children.get(0)));
            root.Eval = childrenEval.get(0).equals("T") ? "F" : "T";
            root.Why.add(root.Formula + " is " +
                    l(root.Eval) + " at " + root.World);
        }
        else if (root.Info.equals(nec)) {
            String separator = "", next = "next(" + root.World +") = {";
            for (String w : root.m.nextWorlds(root.World)) {
                next += separator + w;
                separator = ", ";
            }
            next += "}";
            root.Eval = "T";
            for (KNode n : root.Children)
                if (eval(n).equals("F")) {
                    root.Eval = "F";
                    root.necOrPosRouteNode = n;
                    break;
                }
            root.Why.add(root.Formula + " is " +
                    l(root.Eval) + " at " + root.World);
            root.Why.add(next);
        }
        else if (root.Info.equals(pos)) {
            String separator = "", next = "next(" + root.World +") = {";
            for (String w : root.m.nextWorlds(root.World)) {
                next += separator + w;
                separator = ", ";
            }
            next += "}";
            root.Eval = "F";
            for (KNode n : root.Children)
                if (eval(n).equals("T")) {
                    root.Eval = "T";
                    root.necOrPosRouteNode = n;
                    break;
                }
            root.Why.add(root.Formula + " is " +
                    l(root.Eval) + " at " + root.World);
            root.Why.add(next);
        }
        else {
            childrenEval.add(eval(root.Children.get(0)));
            childrenEval.add(eval(root.Children.get(1)));
            if (root.Info.equals("|")){
                root.Eval = "T";
                if (childrenEval.get(0).equals("T"))
                    root.necOrPosRouteNode = root.Children.get(0);
                else if (childrenEval.get(1).equals("T"))
                    root.necOrPosRouteNode = root.Children.get(1);
                else
                    root.Eval = "F";
            }
            else if (root.Info.equals("&")){
                root.Eval = "F";
                if (childrenEval.get(0).equals("F"))
                    root.necOrPosRouteNode = root.Children.get(0);
                else if (childrenEval.get(1).equals("F"))
                    root.necOrPosRouteNode = root.Children.get(1);
                else
                    root.Eval = "T";
            }
            else if (root.Info.equals(impl)){
                root.Eval = "T";
                if (childrenEval.get(0).equals("F"))
                    root.necOrPosRouteNode = root.Children.get(0);
                else if (childrenEval.get(1).equals("T"))
                    root.necOrPosRouteNode = root.Children.get(1);
                else
                    root.Eval = "F";
            }
            else if (root.Info.equals(iff))
                root.Eval = (childrenEval.get(0).equals(childrenEval.get(1)) ? "T" : "F");
            root.Why.add(root.Formula + " is " +
                    l(root.Eval) + " at " + root.World);
        }
        return root.Eval;
    }
    static String evalFinal(String form, String w, Model m) {
        KNode root = buildTreeFinal(form, m, w);
        return eval(root);
    }

    static String evalWhy(KNode root, int why, String indent) {
        String ret = indent + root.Why.get(0);
        if (why <= 0)
            return ret;
        else {
            if (root.Info.equals(nec) || root.Info.equals(pos)){
                ret += "\n  " + indent + root.Why.get(1);
            }

            if (root.necOrPosRouteNode != null) {
                ret += "\n" + evalWhy (root.necOrPosRouteNode, why - 1, "  " + indent );
            }
            else
                for (KNode child : root.Children)
                    ret += "\n" + evalWhy(child, why - 1, "  " + indent);
        }
        return ret;
    }

    static String evalWhyFinal(String formula, int why, String world, Model m) {
        KNode root = buildTreeFinal(formula, m, world);
        eval(root);
        return evalWhy(root, why, "* ").substring(2);
    }

    static String evalModel(String form, Model m) {
        String ret = "{", sep = "";
        for (String w: m.Worlds) {
            if (evalFinal(form, w, m).equals("T")) {
                ret += sep + w;
                sep = ", ";
            }
        }
        return ret + "}";
    }
}

class Kripke extends Logger {
    Map<String, Model> Models;
    Map<String, String> Formulas;
    String current = "";
    Model currentM;

    Kripke() {
        Models = new HashMap<String, Model>();
        currentM = null;
        Formulas = new HashMap<String, String>();
    }

    void exeFile(String filename) {
        String line ="";
        BufferedReader br = null;
        DataInputStream in = null;
        try {
            FileInputStream fstream = new FileInputStream(filename);
            // Get the object of DataInputStream
            in = new DataInputStream(fstream);
            br = new BufferedReader(new InputStreamReader(in));
            //Read File Line By Line
            line = br.readLine();
        } catch (Exception e){
            System.out.print("ERROR");
            return;
        }
            while ( line != null) {
                executeLine(line);
                try{line = br.readLine();
                        }catch (Exception e){
                            System.out.print("ERROR");
                            return;
                        }
            }
            //Close the input stream
            try{
                in.close();
        }catch (Exception e){
            System.out.print("ERROR");
            return;
        }
    }


    void executeLine(String line) {
        String[] st = line.split(" ");
        if (st.length == 0)
            return;
        if (st.length == 1) {
            if (st[0].equalsIgnoreCase("SHOW")) {
                if (current.equals(""))
                    log("No model is currently open for editing. Use the EDIT command first.");
                else
                    showModel(current);
            }
            else if (st[0].equalsIgnoreCase("CLEAR")) {
                if(!current.equals("")) {
                    currentM = new Model();
                    Models.remove(current);
                    Models.put(current, currentM);
                }
            }
        }
        else if (st.length == 2) {
            if (st[0].equalsIgnoreCase("List")) {
                if (st[1].equalsIgnoreCase("models")) {
                    listModels();
                    return;
                }
            }

            if (currentM == null) {
                log("No model is currently open for editing. Use the EDIT command first.");
                return;
            }
            if (st[0].equalsIgnoreCase("clear")) {
                if (st[1].equalsIgnoreCase("relations")) {
                    currentM.Relations = new LinkedHashSet<Relation>();
                    return;
                }
            }

            if (st[0].equalsIgnoreCase("clear")) {
                if (st[1].equalsIgnoreCase("truth")) {
                    currentM.clearTruth();
                    return;
                }
            }
        } else {
            if (st[0].equalsIgnoreCase("create"))
                if (st[1].equalsIgnoreCase("model")) {
                    create(st[2], (st.length == 4 && st[3].equalsIgnoreCase("force")));
                    return;
                }
            if (st[0].equalsIgnoreCase("create"))
                if (st[1].equalsIgnoreCase("model")) {
                    create(st[2], (st.length == 4 && st[3].equalsIgnoreCase("force")));
                    return;
                }
            if (st[0].equalsIgnoreCase("remove"))
                if (st[1].equalsIgnoreCase("model")) {
                    remove(st[2]);
                    return;
                }
            if (st[0].equalsIgnoreCase("show"))
                if (st[1].equalsIgnoreCase("model")) {
                    showModel(st[2]);
                    return;
                }
            if (st[0].equalsIgnoreCase("show"))
                if (st[1].equalsIgnoreCase("all"))
                    if (st[2].equalsIgnoreCase("models")){
                        showAllModels();
                        return;
                    }
            if (st[0].equalsIgnoreCase("edit"))
                if (st[1].equalsIgnoreCase("model")) {
                    current = st[2];
                    currentM = Models.get(current);
                    return;
                }
            if (st[0].equalsIgnoreCase("load"))
                if (st[1].equalsIgnoreCase("models"))
                    if (st[2].equalsIgnoreCase("from")) {
                        load(st[3]);
                    return;
                }
            if (st[0].equalsIgnoreCase("save"))
                if (st[1].equalsIgnoreCase("models"))
                    if (st[2].equalsIgnoreCase("to")) {
                        save(st[3]);
                    return;
                }
            if (st[0].equalsIgnoreCase("create"))
                if (st[1].equalsIgnoreCase("worlds")) {
                    currentM.addWorlds(Arrays.copyOfRange(st, 2, st.length));
                    return;
                }
            if (st[0].equalsIgnoreCase("create"))
                if (st[1].equalsIgnoreCase("formula")) {
                    Formulas.put(st[2], line.split("=")[1]);
                    return;
                }
            if (st[0].equalsIgnoreCase("show"))
                if (st[1].equalsIgnoreCase("formula")) {
                    disp(showFormula(Formulas.get(st[2])));
                    return;
                }

            if (st[0].equalsIgnoreCase("evaluate"))
                if (st[2].equalsIgnoreCase("in"))
                    if (st.length == 6 && st[4].equals("AT")) {
                        disp( KNode.evalFinal(showFormula(st[1]),
                                st[5], Models.get(st[3])) );
                        return;
                    }
            if (st[0].equalsIgnoreCase("evaluate"))
                if (st[2].equalsIgnoreCase("in"))
                    if (st.length == 4) {
                        KNode.evalModel(showFormula(st[1]), Models.get(st[3]));
                        return;
                    }
            if (st[0].equalsIgnoreCase("evaluate"))
                if (st[2].equalsIgnoreCase("in"))
                    if (st.length == 8 && st[4].equals("AT") &&
                    st[6].equalsIgnoreCase("WHY") && !st[7].equalsIgnoreCase("ALL")) {
                        disp( KNode.evalWhyFinal(
                                showFormula(st[1]),
                                Integer.parseInt(st[7]),
                                st[5],
                                Models.get(st[3])));
                        return;
                    }
            if (st[0].equalsIgnoreCase("evaluate"))
                if (st[2].equalsIgnoreCase("in"))
                    if (st.length == 8 && st[4].equals("AT") &&
                    st[6].equalsIgnoreCase("WHY") && st[7].equalsIgnoreCase("ALL")) {
                        disp( KNode.evalWhyFinal(
                                showFormula(st[1]),
                                100,
                                st[5],
                                Models.get(st[3])));
                        return;
                    }

            if (currentM == null) {
                log("No model is currently open for editing. Use the EDIT command first.");
                return;
            }
            if (st[0].equalsIgnoreCase("add"))
                if (st[1].equalsIgnoreCase("worlds")) {
                    currentM.addWorlds(Arrays.copyOfRange(st, 2, st.length));
                    return;
                }
            if (st[0].equalsIgnoreCase("remove"))
                if (st[1].equalsIgnoreCase("worlds")) {
                    currentM.removeWorlds(Arrays.copyOfRange(st, 2, st.length));
                    return;
                }
            if (st[0].equalsIgnoreCase("rename"))
                if (st[1].equalsIgnoreCase("world")) {
                    currentM.renameWorld(st[2], st[4]);
                    return;
                }
            if (st[0].equalsIgnoreCase("add"))
                if (st[1].equalsIgnoreCase("relations")) {
                    currentM.addRelations(Arrays.copyOfRange(st, 2, st.length));
                    return;
                }
            if (st[0].equalsIgnoreCase("remove"))
                if (st[1].equalsIgnoreCase("relations")) {
                    currentM.removeRelations(Arrays.copyOfRange(st, 2, st.length));
                    return;
                }
            if (st[0].equalsIgnoreCase("add"))
                if (st[1].equalsIgnoreCase("true")) {
                    int i = 2;
                    while (!st[i].equals("AT"))
                        i++;
                    currentM.addTrue(Arrays.copyOfRange(st, 2, i),
                            Arrays.copyOfRange(st, i+1, st.length));
                    return;
                }
            if (st[0].equalsIgnoreCase("remove"))
                if (st[1].equalsIgnoreCase("true")) {
                    int i = 2;
                    while (!st[i].equals("AT"))
                        i++;
                    currentM.removeTrue(Arrays.copyOfRange(st, 2, i),
                            Arrays.copyOfRange(st, i+1, st.length));
                    return;
                }
            if (st[0].equalsIgnoreCase("remove"))
                if (st[1].equalsIgnoreCase("propositions")) {
                    currentM.removeProps(Arrays.copyOfRange(st, 2, st.length));
                    return;
                }
            if (st[0].equalsIgnoreCase("rename"))
                if (st[1].equalsIgnoreCase("proposition")) {
                    currentM.renameProp(st[2], st[4]);
                    return;
                }
        }
    }

    void load(String filename) {

        try {
            FileInputStream fstream = new FileInputStream(filename);
            // Get the object of DataInputStream
            DataInputStream in = new DataInputStream(fstream);
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            //Read File Line By Line
            int i,j, n = Integer.parseInt(br.readLine());
            Models = new HashMap<String, Model>();
            String[] tokens;
            for (i = 0; i < n; i++) {
                tokens = br.readLine().split(" ");
                Model m = new Model();
                String modelName = tokens[1];
                tokens = br.readLine().split(" ");
                int worldsNr = tokens.length - 1;
                for (j = 1; j < tokens.length; j++) {
                    m.Worlds.add(tokens[j]);
                }
                //relation
                StringTokenizer st = new StringTokenizer(br.readLine(), "(,) ");
                st.nextToken();
                while (st.hasMoreTokens()) {
                    m.Relations.add(new Relation(st.nextToken(), st.nextToken()));
                }
                //truths
                Set truthsInWorld;
                for ( j = 0; j < worldsNr; j++) {
                    st = new StringTokenizer(br.readLine(), ": ");
                    st.nextToken();
                    String world = st.nextToken();
                    truthsInWorld = new LinkedHashSet<String>();
                    while (st.hasMoreTokens())
                        truthsInWorld.add(st.nextToken());
                    m.Truths.put(world, truthsInWorld);
                }
                Models.put(modelName, m);
            }
            //Close the input stream
            in.close();
        }catch (Exception e){//Catch exception if any
            System.err.println("File not found. " + e.getMessage());
        }
     }

    void save(String filename) {
        try {
            // Create file
            FileWriter fstream = new FileWriter(filename);
            BufferedWriter out = new BufferedWriter(fstream);
            String ret = "" + Models.size() + "\n";
            for (String m : Models.keySet()) {
                ret += "MODEL " + m + "\nWORLDS";
                for (String w : Models.get(m).Worlds) {
                    ret += " " + w;
                }
                ret += "\nRELATIONS";
                for (Relation r : Models.get(m).Relations) {
                    ret += " (" + r.a + ", " + r.b + ")";
                }
                for (String w : Models.get(m).Worlds) {
                    ret += "\nTRUE " + w + " :";
                    for (String prop : Models.get(m).Truths.get(w)) {
                        ret += " " + prop;
                    }
                }
            }
            out.write(ret);
            //Close the output stream
            out.close();
        } catch (Exception e) {//Catch exception if any
            System.err.println("Error: " + e.getMessage());
        }
    }

    int create(String name, boolean force) {
        if (Models.containsKey(name)) {
            if (force) {
                Models.remove(name);
                Models.put(name, new Model());
                log ("Existing model " + name + " overwritten.");
            }
            else {
                log("Model " + name + " already exists. " +
                        "Use the EDIT command to open this model for editing.");
            }
        }
        else {
            Models.put(name, new Model());
        }
        return 0;
    }

    int remove(String name) {
         if (Models.containsKey(name)) {
             Models.remove(name);
         }
         else
             log("Model " + name + " not found.");
        return 0;
    }

    int showModel(String name) {
        if (Models.containsKey(name)) {
            String show = "Model " + name + ":\n";
            show += Models.get(name).show();
            disp(show);
        }
        else
            log("Model " + name + " not found.");
        return 0;
    }

    int listModels() {
        disp("Models: ");
        for (String m: Models.keySet()) {
            disp(m + " " );
        }
        return 0;
    }

    int showAllModels() {
        for (String m: Models.keySet()) {
            showModel(m);
        }
        return 0;
    }

    void createFormula(String name, String form) {
        StringTokenizer st = new StringTokenizer(form,
                            nec + pos + impl + iff + "|&~()$", true);
        while (st.hasMoreTokens())
            if(st.nextToken().contains("$")) {
                String refForm = st.nextToken();
                if (!Formulas.containsKey(refForm)) {
                    log("Formula " + name + " not found");
                    return;
                }
            }
        Formulas.put(name, form);
    }

    String showFormula(String name) {
        if (!Formulas.containsKey(name)) {
            log ("Formula not found.");
            return "";
        }
        String form = Formulas.get(name);
        StringTokenizer st = new StringTokenizer(form,
                            nec + pos + impl + iff + "|&~()$", true);
        form = "";
        String token;
        while (st.hasMoreTokens()) {
            token = st.nextToken();
            if (token.equals("$")) {
                form += "(" + showFormula(st.nextToken()) + ")";
            }
            else form += token;
        }
        return KNode.buildTreeFinal(form, null, null).Formula;
    }

}

public class Main {

    public static void main(String[] args) {
        Kripke a = new Kripke();
        Scanner sc = new Scanner(System.in);
        //while (sc.hasNextLine()) {
            String line = "execute commands from e:/commands.txt";//sc.nextLine();

            String[] st = line.split(" ");
            if (st.length == 4 && st[0].equalsIgnoreCase("execute") &&
                    st[1].equalsIgnoreCase("commands") && st[2].equalsIgnoreCase("from"))
                a.exeFile(st[3]);
            else
                a.executeLine(st[3]);
        //}

    }
}
