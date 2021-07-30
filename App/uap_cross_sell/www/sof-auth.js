
const config = {
  apiKey: "AIzaSyDmD0gLfssWK9w6vsaoyHFuIBllYTE_dnQ",
  authDomain: "localhost",
  projectId: "uap-cross-sell-v1"
}

firebase.initializeApp(config)

const auth = firebase.auth()

$(document).on("click", "#submit_sign_in", () => {
  const email = $("#email").val()
  const password = $("#password").val()

  auth.signInWithEmailAndPassword(email, password)
  .catch((error) => {
    showSnackbar("sign_in_snackbar", "Error: " + error.message)
    console.log("sign in error: ", error)
  })
})



$(document).on("click", "#submit_sign_out", () => {
  auth.signOut()
  .catch((error) => {
    console.log("Sign Out Error", error)
  })
})

$(document).on("click", "#verify_email_submit_sign_out", () => {
  auth.signOut()
  .catch((error) => {
    console.log("Sign Out Error", error)
  })
})

auth.onAuthStateChanged((user) => {
  // when user signs in or out send the info about that user to Shiny as
  // a Shiny input `input$sof_auth_user`
  Shiny.setInputValue('sof_auth_user', user);
})

$(document).on("click", "#resend_email_verification", () => {

  const user = auth.currentUser

  user.sendEmailVerification()
  .then(() => {
    showSnackbar("verify_email_snackbar", "verification email send to " + user.email)
  })
  .catch((error) => {
    showSnackbar("verify_email_snackbar", "Error: " + error.message)
    console.error('error sending email verification', error)
  })
})



$(document).on("click", "#submit_register", () => {

  const email = $("#register_email").val()
  
  const password = $("#register_password").val()
  const password_2 = $("#register_password_verify").val()
  console.log(
    "submit_register",
    email,
    password,
    password_2
  )

// password standards
// let uap_doms = /gmail\.com$|yahoo\.com$/g;
let uap_doms = /uapoldmutual\.com$|oldmutual\.com$|oldmutual\.co\.zw$/g;
let pass_std = /^(?=.*[a-z])(?=.*[A-Z])(?=.*[0-9])(?=.*[!@#\$%\^&\*])(?=.{8,})/;

  if (password === password_2 && uap_doms.test(email) && pass_std.test(password)) {
    auth.createUserWithEmailAndPassword(email, password).then((user) => {

      user.user.sendEmailVerification()
      .catch((error) => {
        showSnackbar("register_snackbar", "Error: " + error.message)
        console.log("error sending email verification: ", error)
      })

    })
    .catch((error) => {
      showSnackbar("register_snackbar", "Error: " + error.message)
    })

  } else {
    showSnackbar("register_snackbar", "Your password should be at least \
    8 character long and contain at least one: \
    uppercase and lower case character, \
    number, \
    special character")
  }
})

$(document).on("click", "#reset_password", () => {
  const email = $("#email").val()

  console.log("reset password ran")
  auth.sendPasswordResetEmail(email).then(() => {
    showSnackbar("sign_in_snackbar", "Reset email sent to " + email)
  }).catch((error) => {
    showSnackbar("sign_in_snackbar", "Error: " + error.message)
    console.log("error resetting email: ", error)

  })
})
