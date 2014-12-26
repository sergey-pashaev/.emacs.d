;;; helm-meyers.el --- Don't forget Meyers rules!

(require-or-install 'helm)

(defvar helm-meyers-buffer-name "*helm meyers*")
(defvar helm-meyers-mode-name " Helm meyers")
(defvar helm-meyers-mode-map (make-sparse-keymap))

(setq helm-meyers-effective-cpp-rules
  '(("Item 1: View C++ as a federation of languages" .
     (1 "Item 1: View C++ as a federation of languages:
- Rules for effective C++ programming vary, depending on
the part of C++ you are using.  depending on the part of C++ you
are using."))

    ("Item 2: Prefer consts, enums, and inlines to #defines" .
     (2 "Item 2: Prefer consts, enums, and inlines to #defines:
- For simple constants, prefer const objects or enums to #defines.
- For function-like macros, prefer inline functions to #defines."))

    ("Item 3: Use const whenever possible" .
     (3 "Item 3: Use const whenever possible:"))

    ("Item 4: Make sure that objects are initialized before they're used" .
     (4 "Item 4: Make sure that objects are initialized before they're used:"))

    ("Item 5: Know what functions C++ silently writes and calls" .
     (5 "Item 5: Know what functions C++ silently writes and calls:"))

    ("Item 6: Explicitly disallow the use of compiler-generated functions you do not want" .
     (6 "Item 6: Explicitly disallow the use of compiler-generated functions you do not want:"))

    ("Item 7: Declare destructors virtual in polymorphic base classes" .
     (7 "Item 7: Declare destructors virtual in polymorphic base classes:"))

    ("Item 8: Prevent exceptions from leaving destructors" .
     (8 "Item 8: Prevent exceptions from leaving destructors:"))

    ("Item 9: Never call virtual functions during construction or destruction" .
     (9 "Item 9: Never call virtual functions during construction or destruction:"))

    ("Item 10: Have assignment operators return a reference to *this" .
     (10 "Item 10: Have assignment operators return a reference to *this:"))

    ("Item 11: Handle assignment to self in operator=" .
     (11 "Item 11: Handle assignment to self in operator=:"))

    ("Item 12: Copy all parts of an object" .
     (12 "Item 12: Copy all parts of an object:"))

    ("Item 13: Use objects to manage resources." .
     (13 "Item 13: Use objects to manage resources.:"))

    ("Item 14: Think carefully about copying behavior in resource-managing classes." .
     (14 "Item 14: Think carefully about copying behavior in resource-managing classes.:"))

    ("Item 15: Provide access to raw resources in resource-managing classes." .
     (15 "Item 15: Provide access to raw resources in resource-managing classes.:"))

    ("Item 16: Use the same form in corresponding uses of new and delete." .
     (16 "Item 16: Use the same form in corresponding uses of new and delete.:"))

    ("Item 17: Store newed objects in smart pointers in standalone statements." .
     (17 "Item 17: Store newed objects in smart pointers in standalone statements.:"))

    ("Item 18: Make interfaces easy to use correctly and hard to use incorrectly" .
     (18 "Item 18: Make interfaces easy to use correctly and hard to use incorrectly:"))

    ("Item 19: Treat class design as type design" .
     (19 "Item 19: Treat class design as type design:"))

    ("Item 20: Prefer pass-by-reference-to-const to pass-by-value" .
     (20 "Item 20: Prefer pass-by-reference-to-const to pass-by-value:"))

    ("Item 21: Don't try to return a reference when you must return an object" .
     (21 "Item 21: Don't try to return a reference when you must return an object:"))

    ("Item 22: Declare data members private" .
     (22 "Item 22: Declare data members private:"))

    ("Item 23: Prefer non-member non-friend functions to member functions" .
     (23 "Item 23: Prefer non-member non-friend functions to member functions:"))

    ("Item 24: Declare non-member functions when type conversions should apply to all parameters" .
     (24 "Item 24: Declare non-member functions when type conversions should apply to all parameters:"))

    ("Item 25: Consider support for a non-throwing swap" .
     (25 "Item 25: Consider support for a non-throwing swap:"))

    ("Item 26: Postpone variable definitions as long as possible." .
     (26 "Item 26: Postpone variable definitions as long as possible.:"))

    ("Item 27: Minimize casting." .
     (27 "Item 27: Minimize casting.:"))

    ("Item 28: Avoid returning \"handles\" to object internals." .
     (28 "Item 28: Avoid returning \"handles\" to object internals.:"))

    ("Item 29: Strive for exception-safe code." .
     (29 "Item 29: Strive for exception-safe code.:"))

    ("Item 30: Understand the ins and outs of inlining." .
     (30 "Item 30: Understand the ins and outs of inlining.:"))

    ("Item 31: Minimize compilation dependencies between files." .
     (31 "Item 31: Minimize compilation dependencies between files.:"))

    ("Item 32: Make sure public inheritance models \"is-a.\"" .
     (32 "Item 32: Make sure public inheritance models \"is-a.\":"))

    ("Item 33: Avoid hiding inherited names" .
     (33 "Item 33: Avoid hiding inherited names:"))

    ("Item 34: Differentiate between inheritance of interface and inheritance of implementation" .
     (34 "Item 34: Differentiate between inheritance of interface and inheritance of implementation:"))

    ("Item 35: Consider alternatives to virtual functions" .
     (35 "Item 35: Consider alternatives to virtual functions:"))

    ("Item 36: Never redefine an inherited non-virtual function" .
     (36 "Item 36: Never redefine an inherited non-virtual function:"))

    ("Item 37: Never redefine a function's inherited default parameter value" .
     (37 "Item 37: Never redefine a function's inherited default parameter value:"))

    ("Item 38: Model \"has-a\" or \"is-implemented-in-terms-of\" through composition" .
     (38 "Item 38: Model \"has-a\" or \"is-implemented-in-terms-of\" through composition:"))

    ("Item 39: Use private inheritance judiciously" .
     (39 "Item 39: Use private inheritance judiciously:"))

    ("Item 40: Use multiple inheritance judiciously" .
     (40 "Item 40: Use multiple inheritance judiciously:"))

    ("Item 41: Understand implicit interfaces and compile-time polymorphism" .
     (41 "Item 41: Understand implicit interfaces and compile-time polymorphism:"))

    ("Item 42: Understand the two meanings of typename" .
     (42 "Item 42: Understand the two meanings of typename:"))

    ("Item 43: Know how to access names in templatized base classes" .
     (43 "Item 43: Know how to access names in templatized base classes:"))

    ("Item 44: Factor parameter-independent code out of templates" .
     (44 "Item 44: Factor parameter-independent code out of templates:"))

    ("Item 45: Use member function templates to accept \"all compatible types.\"" .
     (45 "Item 45: Use member function templates to accept \"all compatible types.\":"))

    ("Item 46: Define non-member functions inside templates when type conversions are desired" .
     (46 "Item 46: Define non-member functions inside templates when type conversions are desired:"))

    ("Item 47: Use traits classes for information about types" .
     (47 "Item 47: Use traits classes for information about types:"))

    ("Item 48: Be aware of template metaprogramming" .
     (48 "Item 48: Be aware of template metaprogramming:"))

    ("Item 49: Understand the behavior of the new-handler" .
     (49 "Item 49: Understand the behavior of the new-handler:"))

    ("Item 50: Understand when it makes sense to replace new and delete" .
     (50 "Item 50: Understand when it makes sense to replace new and delete:"))

    ("Item 51: Adhere to convention when writing new and delete" .
     (51 "Item 51: Adhere to convention when writing new and delete:"))

    ("Item 52: Write placement delete if you write placement new" .
     (52 "Item 52: Write placement delete if you write placement new:"))

    ("Item 53: Pay attention to compiler warnings." .
     (53 "Item 53: Pay attention to compiler warnings.:"))

    ("Item 54: Familiarize yourself with the standard library, including TR1" .
     (54 "Item 54: Familiarize yourself with the standard library, including TR1:"))

    ("Item 55: Familiarize yourself with Boost." .
     (55 "Item 55: Familiarize yourself with Boost.:"))))

(defun helm-meyers--action (desc)
  (pop-to-buffer (generate-new-buffer (format "*helm-meyers-%d-item*" (car desc))))
  (insert (car (cdr desc)))
  (compilation-mode))

(defmacro helm-meyers--source (source-name table)
  `'((name . ,source-name)
     (candidates . ,table)
     (action . helm-meyers--action)))

(defun helm-meyers ()
  (interactive)
  (helm :sources (list (helm-meyers--source "effective c++" helm-meyers-effective-cpp-rules))
        :buffer helm-meyers-buffer-name))

(define-minor-mode helm-meyers-mode ()
  "Enable for helm-meyers"
  :group      'helm-meyers
  :init-value nil
  :global     nil
  :keymap     helm-meyers-mode-map
  :lighter    helm-meyers-mode-name
  (if helm-meyers-mode
      (run-hooks 'helm-meyers-mode-hook)))

(provide 'helm-meyers)

;;; helm-meyers.el ends here
