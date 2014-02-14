import unittest
import tempfile
import os

from elpy.utils import autoimport


class TestGetChanges(unittest.TestCase):
    def setUp(self):
        self.project_root = tempfile.mkdtemp(prefix="test-autoimport")

    def create_file(self, filename, contents):
        fullname = os.path.join(self.project_root, filename)
        with open(fullname, "w") as f:
            f.write(contents)
        return fullname

    def test_should_do_nothing_for_global_names(self):
        filename = self.create_file("foo.py",
                                    "open('foo').read()")
        self.assertEqual(autoimport.get_changes(filename),
                         [])

    def test_should_add_imports_with_no_prior_imports(self):
        filename = self.create_file("foo.py",
                                    "obj = json.dumps(23)")
        (change,) = autoimport.get_changes(filename)
        self.assertEqual(change['action'], 'change')
        self.assertEqual(change['file'], filename)
        self.assertEqual(change['contents'],
                         "import json\n"
                         "obj = json.dumps(23)")

    def test_should_add_imports_after_other_imports(self):
        filename = self.create_file("foo.py",
                                    "import elpy\n"
                                    "obj = json.dumps(23)\n")
        (change,) = autoimport.get_changes(filename)
        self.assertEqual(change['action'], 'change')
        self.assertEqual(change['file'], filename)
        self.assertEqual(change['contents'],
                         "import elpy\n"
                         "import json\n"
                         "obj = json.dumps(23)\n")

    def test_should_not_add_duplicates(self):
        filename = self.create_file("foo.py",
                                    "if sys.argv[1]:\n"
                                    "    sys.exit('yiakes')\n")
        (change,) = autoimport.get_changes(filename)
        self.assertEqual(change['action'], 'change')
        self.assertEqual(change['file'], filename)
        self.assertEqual(change['contents'],
                         "import sys\n"
                         "if sys.argv[1]:\n"
                         "    sys.exit('yiakes')\n")


class TestFindModuleFor(unittest.TestCase):
    def test_should_find_direct_module(self):
        self.assertEqual(autoimport.find_module_for("json"),
                         "json")
        self.assertEqual(autoimport.find_module_for("xml.etree"),
                         "xml.etree")

    def test_should_find_prefix_module(self):
        self.assertEqual(autoimport.find_module_for
                         ("json.dumps"),
                         "json")
        self.assertEqual(autoimport.find_module_for
                         ("xml.etree.ElementTree.parse"),
                         "xml.etree.ElementTree")

    def test_should_return_none_for_bad_module(self):
        self.assertIsNone(autoimport.find_module_for("foo"))
        self.assertIsNone(autoimport.find_module_for("foo.bar"))


class TestAddImports(unittest.TestCase):
    def test_should_add_imports_at_beginning_of_empty_file(self):
        self.assertEqual(autoimport.add_imports("", ["foo", "bar"]),
                         "import foo\nimport bar\n")

    def test_should_add_imports_at_end_of_existing_imports(self):
        source = ("import x\n"
                  "import y\n"
                  "\n"
                  "meep.meep()\n")
        self.assertEqual(autoimport.add_imports(source, ["foo", "bar"]),
                         ("import x\n"
                          "import y\n"
                          "import foo\n"
                          "import bar\n"
                          "\n"
                          "meep.meep()\n"))
