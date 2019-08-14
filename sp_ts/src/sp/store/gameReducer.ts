export const LOGIN_ATTEMPT = "LOGIN_ATTEMPT";

const initState: { isLoggedIn: boolean } = {
  isLoggedIn: false
};

export const toggleLoginAttempt = () => ({
  type: LOGIN_ATTEMPT
});

export const gameReducer = (
  state = initState,
  action: { type: string; payload?: any }
) => {
  console.log("Action:", action);
  switch (action.type) {
    case LOGIN_ATTEMPT:
      return { ...state, isLoggedIn: !state.isLoggedIn };

    default:
      return state;
  }
};