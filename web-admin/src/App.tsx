import { Routes, Route, Navigate, useLocation } from "react-router-dom";
import { getToken } from "./api/client";
import Login from "./pages/Login";
import UsersList from "./pages/UsersList";
import UserEdit from "./pages/UserEdit";
import UserTokens from "./pages/UserTokens";
import Layout from "./components/Layout";

function Guard({ children }: { children: React.ReactNode }) {
  const loc = useLocation();
  if (!getToken()) return <Navigate to="/login" state={{ from: loc }} replace />;
  return <>{children}</>;
}

export default function App() {
  return (
    <Routes>
      <Route path="/login" element={<Login />} />
      <Route
        path="/"
        element={
          <Guard>
            <Layout />
          </Guard>
        }
      >
        <Route index element={<Navigate to="users" replace />} />
        <Route path="users" element={<UsersList />} />
        <Route path="users/:id" element={<UserEdit />} />
        <Route path="users/:id/tokens" element={<UserTokens />} />
      </Route>
      <Route path="*" element={<Navigate to="/" replace />} />
    </Routes>
  );
}
